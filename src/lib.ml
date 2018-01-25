open Core

let raise_runtime_error msg = raise (Eval.Runtime_error (None, msg))
let raise_type_error ~expect ~actual = raise_runtime_error (
    "expected: " ^ expect ^ "\n" ^
    "actual  : " ^ actual
  )

(* constants *)
module C = struct
  open Value

  let pi      = Float.pi
  let e       = Float.exp 1.0
  let ln2     = Float.log 2.0
  let ln10    = Float.log 10.0
  let log2e   = 1.0 /. ln2
  let log10e  = 1.0 /. ln10
  let sqrt2   = Float.sqrt 2.0
  let sqrt1_2 = 1.0 /. sqrt2

  let v_pi      = Num pi
  let v_e       = Num e
  let v_ln2     = Num ln2
  let v_ln10    = Num ln10
  let v_log2e   = Num log2e
  let v_log10e  = Num log10e
  let v_sqrt2   = Num sqrt2
  let v_sqrt1_2 = Num sqrt1_2
end


(* unary operators *)
let make_unary_op f =
  let open Value in
  let rec op = function
    | Num num -> Num (f num)
    | Vec vec -> Vec (Array.map vec ~f:op)
    | Fun _   -> raise_type_error ~expect:"number or vector" ~actual:"function"
  in
  Fun op

module U = struct
  let sign x =
    let open Float.Sign_or_nan in
    match Float.sign_or_nan x with
    | Pos -> 1.0
    | Neg -> -1.0
    | Zero -> 0.0
    | Nan -> Float.nan

  let v_plus  = make_unary_op Fn.id
  let v_minus = make_unary_op Float.neg
  let v_sign  = make_unary_op sign
  let v_abs   = make_unary_op Float.abs
  let v_round = make_unary_op Float.round_nearest
  let v_floor = make_unary_op Float.round_down
  let v_ceil  = make_unary_op Float.round_up
  let v_sqrt  = make_unary_op Float.sqrt
  let v_exp   = make_unary_op Float.exp
  let v_expm1 = make_unary_op Float.expm1
  let v_log   = make_unary_op Float.log
  let v_log1p = make_unary_op Float.log1p
  let v_log2  = make_unary_op (fun x -> Float.log x /. C.ln2)
  let v_log10 = make_unary_op Float.log10
  let v_sin   = make_unary_op Float.sin
  let v_cos   = make_unary_op Float.cos
  let v_tan   = make_unary_op Float.tan
  let v_asin  = make_unary_op Float.asin
  let v_acos  = make_unary_op Float.acos
  let v_atan  = make_unary_op Float.atan
  let v_sinh  = make_unary_op Float.sinh
  let v_cosh  = make_unary_op Float.cosh
  let v_tanh  = make_unary_op Float.tanh
end


(* binary operators *)
let make_binary_op f =
  let open Value in
  let rec op = function
    | Fun _ -> raise_type_error ~expect:"number or vector" ~actual:"function"
    | x -> function
      | Fun _ -> raise_type_error ~expect:"number or vector" ~actual:"function"
      | y -> match (x, y) with
        | (Fun _, _) | (_, Fun _) -> raise_type_error ~expect:"number or vector" ~actual:"function"
        | (Num num1, Num num2) -> Num (f num1 num2)
        | (Num _, Vec vec) -> Vec (Array.map vec ~f:(fun elem -> op x elem))
        | (Vec vec, Num _) -> Vec (Array.map vec ~f:(fun elem -> op elem y))
        | (Vec vec1, Vec vec2) -> try Vec (Array.map2_exn vec1 vec2 ~f:op) with
          | Invalid_argument _ -> raise_runtime_error "operating on vectors with unequal lengths"
  in
  Fun (fun x -> Fun (op x))

module B = struct
  let v_add   = make_binary_op ( +. )
  let v_sub   = make_binary_op ( -. )
  let v_mul   = make_binary_op ( *. )
  let v_div   = make_binary_op ( /. )
  let v_mod   = make_binary_op Float.mod_float
  let v_pow   = make_binary_op ( ** )
  let v_log_  = make_binary_op (fun x y -> Float.log y /. Float.log x)
  let v_atan2 = make_binary_op Float.atan2
end


(* vector operations *)
module V = struct
  open Value

  let at vec index = try Array.get vec index with
    | Invalid_argument _ -> raise_runtime_error ("index out of bounds: " ^ Int.to_string index)

  let v_at = Fun (function
      | Vec vec -> Fun (function
          | Num num ->
            begin
              match Float.iround_down num with
              | Some index -> at vec index
              | None -> raise_runtime_error ("invalid index: " ^ Float.to_string num)
            end
          | v -> raise_type_error ~expect:"number" ~actual:(type_string_of v)
        )
      | v -> raise_type_error ~expect:"vector" ~actual:(type_string_of v)
    )

  let v_len = Fun (function
      | Vec vec -> Num (Float.of_int (Array.length vec))
      | v -> raise_type_error ~expect:"vector" ~actual:(type_string_of v)
    )

  let v_fst = Fun (function
      | Vec vec ->
        if Array.is_empty vec then raise_runtime_error "empty vector"
        else Array.get vec 0
      | v -> raise_type_error ~expect:"vector" ~actual:(type_string_of v)
    )

  let v_append = Fun (function
      | Vec vec1 -> Fun (function
          | Vec vec2 -> Vec (Array.append vec1 vec2)
          | v -> raise_type_error ~expect:"vector" ~actual:(type_string_of v)
        )
      | v -> raise_type_error ~expect:"number" ~actual:(type_string_of v)
    )

  let v_take = Fun (function
      | Num num -> Fun (function
          | Vec vec ->
            begin
              match Float.iround_down num with
              | Some n ->
                let len = Array.length vec in
                if n <= 0 then Vec [||]
                else if n > len then Vec vec
                else Vec (Array.slice vec 0 n)
              | None -> raise_runtime_error ("invalid number: " ^ Float.to_string num)
            end
          | v -> raise_type_error ~expect:"vector" ~actual:(type_string_of v)
        )
      | v -> raise_type_error ~expect:"number" ~actual:(type_string_of v)
    )

  let v_drop = Fun (function
      | Num num -> Fun (function
          | Vec vec ->
            begin
              match Float.iround_down num with
              | Some n ->
                let len = Array.length vec in
                if n <= 0 then Vec vec
                else if n > len then Vec [||]
                else Vec (Array.slice vec n len)
              | None -> raise_runtime_error ("invalid number: " ^ Float.to_string num)
            end
          | v -> raise_type_error ~expect:"vector" ~actual:(type_string_of v)
        )
      | v -> raise_type_error ~expect:"number" ~actual:(type_string_of v)
    )
end


(* numeric vector operators *)
let to_float = function
  | Value.Num num -> num
  | v -> raise_type_error ~expect:"number" ~actual:(Value.type_string_of v)

let to_float_array = function
  | Value.Vec vec -> Array.map vec ~f:to_float
  | v -> raise_type_error ~expect:"vector" ~actual:(Value.type_string_of v)

let make_accum_op f = Value.Fun (fun v -> Value.Num (to_float_array v |> f))
let make_accum_op2 f =
  Value.Fun (fun v1 ->
      Value.Fun (fun v2 ->
          Value.Num (f (to_float_array v1) (to_float_array v2))
        )
    )
let make_vec_op f =
  Value.Fun (fun v ->
    Value.Vec (to_float_array v |> f |> Array.map ~f:(fun n -> Value.Num n))
  )

module NV = struct
  let sum vec =
    Array.fold vec
      ~init:(0.0, 0.0)
      ~f:(fun (s, c) x ->
          let y = x -. c in
          let t = s +. y in
          (t, (t -. s) -. y)
        )
    |> fst

  let avg vec = sum vec /. Float.of_int (Array.length vec)

  let square_sum vec =
    let len = Array.length vec in
    if len = 0 then Float.nan
    else
      let mean = avg vec in
      Array.fold vec
        ~init:(0.0, 0.0)
        ~f:(fun (s, c) x ->
            let y = (x -. mean) *. (x -. mean) -. c in
            let t = s +. y in
            (t, (t -. s) -. y)
          )
      |> fst

  let maxi vec =
    Array.foldi vec ~init:(-1, -.Float.infinity)
      ~f:(fun i (maxi, m) x -> if x > m then (i, x) else (maxi, m))
    |> fst
    |> Float.of_int

  let mini vec =
    Array.foldi vec ~init:(-1, Float.infinity)
      ~f:(fun i (mini, m) x -> if x < m then (i, x) else (mini, m))
    |> fst
    |> Float.of_int

  let var vec = square_sum vec /. Float.of_int (Array.length vec - 1)

  let sd vec = Float.sqrt (var vec)
  let se vec = Float.sqrt (var vec /. Float.of_int (Array.length vec))

  let square_sum2 vec1 vec2 =
    let len1 = Array.length vec1 in
    let len2 = Array.length vec2 in
    if len1 <> len2 then raise_runtime_error "operating on vectors with unequal lengths"
    else if len1 = 0 then Float.nan
    else
      let mean1 = avg vec1 in
      let mean2 = avg vec2 in
      Array.fold2_exn vec1 vec2
        ~init:(0.0, 0.0)
        ~f:(fun (s, c) x1 x2 ->
            let y = (x1 -. mean1) *. (x2 -. mean2) -. c in
            let t = s +. y in
            (t, (t -. s) -. y)
          )
      |> fst

  let cov vec1 vec2 = square_sum2 vec1 vec2 /. Float.of_int (Array.length vec1 - 1)

  let cor vec1 vec2 = square_sum2 vec1 vec2 /. (sqrt (square_sum vec1) *. sqrt (square_sum vec2))

  let v_sum   = make_accum_op sum
  let v_prod  = make_accum_op (Array.fold ~init:1.0 ~f:( *. ))
  let v_max   = make_accum_op (Array.fold ~init:(-.Float.infinity) ~f:Float.max)
  let v_min   = make_accum_op (Array.fold ~init:Float.infinity ~f:Float.min)
  let v_maxi  = make_accum_op maxi
  let v_mini  = make_accum_op mini
  let v_avg   = make_accum_op avg
  let v_var   = make_accum_op var
  let v_sd    = make_accum_op sd
  let v_se    = make_accum_op se
  let v_cov   = make_accum_op2 cov
  let v_cor   = make_accum_op2 cor
  let v_asort = make_vec_op (Array.sorted_copy ~cmp:Float.compare)
  let v_dsort = make_vec_op (Array.sorted_copy ~cmp:(fun a b -> - Float.compare a b))
end


(* standard library *)
let std = Eval.Context.of_alist [
    ("pi"     , C.v_pi);
    ("e"      , C.v_e);
    ("ln2"    , C.v_ln2);
    ("ln10"   , C.v_ln10);
    ("log2e"  , C.v_log2e);
    ("log10e" , C.v_log10e);
    ("sqrt2"  , C.v_sqrt2);
    ("sqrt1_2", C.v_sqrt1_2);

    ("+_"   , U.v_plus);
    ("-_"   , U.v_minus);
    ("sign" , U.v_sign);
    ("abs"  , U.v_abs);
    ("round", U.v_round);
    ("floor", U.v_floor);
    ("ceil" , U.v_ceil);
    ("sqrt" , U.v_sqrt);
    ("exp"  , U.v_exp);
    ("expm1", U.v_expm1);
    ("log"  , U.v_log);
    ("log1p", U.v_log1p);
    ("log2" , U.v_log2 );
    ("log10", U.v_log10);
    ("sin"  , U.v_sin);
    ("cos"  , U.v_cos);
    ("tan"  , U.v_tan);
    ("asin" , U.v_asin);
    ("acos" , U.v_acos);
    ("atan" , U.v_atan);
    ("sinh" , U.v_sinh);
    ("cosh" , U.v_cosh);
    ("tanh" , U.v_tanh);

    ("_+_"  , B.v_add);
    ("_-_"  , B.v_sub);
    ("_*_"  , B.v_mul);
    ("_/_"  , B.v_div);
    ("_%_"  , B.v_mod);
    ("_**_" , B.v_pow);
    ("_^_"  , B.v_pow);
    ("log_" , B.v_log_);
    ("atan2", B.v_atan2);

    ("_!_" , V.v_at);
    ("len" , V.v_len);
    ("fst" , V.v_fst);
    ("_@_" , V.v_append);
    ("take", V.v_take);
    ("drop", V.v_drop);

    ("sum"  , NV.v_sum);
    ("prod" , NV.v_prod);
    ("max"  , NV.v_max);
    ("min"  , NV.v_min);
    ("maxi" , NV.v_maxi);
    ("mini" , NV.v_mini);
    ("avg"  , NV.v_avg);
    ("var"  , NV.v_var);
    ("sd"   , NV.v_sd);
    ("se"   , NV.v_se);
    ("cov"  , NV.v_cov);
    ("cor"  , NV.v_cor);
    ("asort", NV.v_asort);
    ("dsort", NV.v_dsort);
  ]
