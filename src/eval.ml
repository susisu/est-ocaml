open Core

exception Runtime_error of string option * string

module Context = struct
  type t = Value.t String.Map.t

  let empty = String.Map.empty
  let of_alist = String.Map.of_alist_reduce ~f:(fun _ x -> x)
  let append = Map.merge ~f:(fun ~key:_ v ->
      match v with
      | `Both (_, x) -> Some x
      | `Left x -> Some x
      | `Right x -> Some x
    )
end

module Make_eval(Info : Common.Showable) = struct
  let raise_runtime_error info msg = raise (Runtime_error (Some (Info.to_string info), msg))

  let rec eval ctx = function
    | Term.Lit (_, v) -> v
    | Term.Var (info, name) ->
      begin
        match Map.find ctx name with
        | Some v -> v
        | None -> raise_runtime_error info ("unbound variable: " ^ name)
      end
    | Term.Vec (_, elems) -> Value.Vec (List.map elems ~f:(eval ctx) |> Array.of_list)
    | Term.App (_, func, arg) ->
      begin
        match eval ctx func with
        | Value.Fun f ->
          let a = eval ctx arg in
          begin
            try f a with
            | Runtime_error (Some _, _) as err -> raise err
            | Runtime_error (None, msg) -> raise_runtime_error (Term.get_info arg) msg
          end
        | v -> raise_runtime_error (Term.get_info func) (
            "expected: function\n" ^
            "actual  : " ^ Value.type_string_of v
          )
      end
    | Term.Let (_, name, expr, body) ->
      let ctx' = Map.add ctx ~key:name ~data:(eval ctx expr) in
      eval ctx' body
end
