val make_unary_op : (float -> float) -> Value.t
val make_binary_op : (float -> float -> float) -> Value.t
val make_accum_op : (float array -> float) -> Value.t
val make_accum_op2 : (float array -> float array -> float) -> Value.t
val std : Eval.Context.t
