open Core

module type Base = sig
  type options

  val read : options -> string -> Eval.Context.t
end

module Make_reader(B : Base) = struct
  type options = B.options

  let read = B.read

  let read_from_channel opts ch = In_channel.input_all ch |> read opts
end


let split_lines str = String.split_lines str
                      |> List.filter ~f:(fun line -> line <> "")

let split_words str = String.split_on_chars ~on:[' '; '\t'] str
                      |> List.filter ~f:(fun word -> word <> "")

let read_num str =
  let num = try Float.of_string str with
    | Invalid_argument _ -> Float.nan
  in
  Value.Num num

let read_line line = split_words line |> List.map ~f:read_num

let create_table lines =
  let lines = List.map lines ~f:read_line in
  let width = List.fold lines ~init:0 ~f:(fun w line -> Int.max w (List.length line)) in
  List.map lines ~f:(fun line ->
      let row = Array.create ~len:width (Value.Num Float.nan) in
      List.iteri line ~f:(fun i v -> row.(i) <- v);
      row
    )
  |> Array.of_list

let create_ctx_alist id table =
  let table_v = Value.Vec table in
  let ctx_alist =
    if id = 0 then Array.to_list table
                   |> List.mapi ~f:(fun i v -> ("$" ^ Int.to_string i, v))
    else []
  in
  let ctx_alist' =
    if id = 0 then ("$$", table_v) :: ("$$0", table_v) :: ctx_alist
    else ("$$" ^ Int.to_string id, table_v) :: ctx_alist
  in
  ctx_alist'


type table_options = {
  id: int;
  transpose: bool;
}

module Table = struct
  type options = table_options

  let remove_comments lines =
    List.filter lines ~f:(fun line -> String.prefix line 1 <> "#")

  let read opts src =
    let transpose = if opts.transpose then Fn.id else Array.transpose_exn in
    let table = split_lines src
                |> remove_comments
                |> create_table
                |> transpose
                |> Array.map ~f:(fun row -> Value.Vec row)
    in
    let ctx_alist = create_ctx_alist opts.id table in
    Eval.Context.of_alist ctx_alist
end


module Table_extended = struct
  type options = table_options

  let valid_name_re = Re2.Regex.create_exn "^[A-Za-z\\$][A-Za-z0-9\\$_']*$"
  let validate_name name = Re2.Regex.matches valid_name_re name

  let read_const consts str = match split_words str with
    | [name; value] -> if validate_name name then
        Hashtbl.set consts ~key:name ~data:(read_num value)
    | _ -> ()

  let read_and_remove_comments consts lines =
    List.filter lines ~f:(fun line ->
        if String.prefix line 1 <> "#" then true
        else begin
          if String.prefix line 2 = "##" then read_const consts (String.drop_prefix line 2);
          false
        end
      )

  let read opts src =
    let transpose = if opts.transpose then Fn.id else Array.transpose_exn in
    let consts = Hashtbl.create ~hashable:String.hashable () in
    let table = split_lines src
                |> read_and_remove_comments consts
                |> create_table
                |> transpose
                |> Array.map ~f:(fun row -> Value.Vec row)
    in
    let ctx_alist = create_ctx_alist opts.id table @ Hashtbl.to_alist consts in
    Eval.Context.of_alist ctx_alist
end
