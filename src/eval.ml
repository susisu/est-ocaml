open Core

exception Runtime_error of string

module Context = struct
  type t = Value.t String.Map.t

  let empty = String.Map.empty
  let of_alist = String.Map.of_alist_reduce ~f:(fun _ x -> x)
end

module Make_eval(Info : Stringable.S) = struct
  let raise_runtime_error info msg =
    let msg' = Info.to_string info ^ ":\n" ^ msg in
    raise (Runtime_error msg')

  let rec eval ctx = function
    | Term.Lit (_, v) -> v
    | Term.Var (info, name) ->
      begin
        match Map.find ctx name with
        | Some v -> v
        | None -> raise_runtime_error info (
            "  unbound variable: " ^ name
          )
      end
    | Term.Vec (_, elems) -> Value.Vec (List.map elems ~f:(eval ctx))
    | Term.App (info, func, arg) ->
      begin
        match eval ctx func with
        | Value.Fun f ->
          let a = eval ctx arg in
          begin
            try f a with
            | Runtime_error msg -> raise_runtime_error info msg
          end
        | v -> raise_runtime_error (Term.get_info func) (
              "  expected: function\n"
            ^ "  actual  : " ^ Value.type_string_of v
          )
      end
    | Term.Let (_, name, expr, body) ->
      let ctx' = Map.add ctx ~key:name ~data:(eval ctx expr) in
      eval ctx' body
end
