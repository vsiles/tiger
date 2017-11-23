type entry =
    | VarEntry of Types.t * bool
    | FunEntry of Types.t list * Types.t

val base_venv : entry Symbol.Table.t
val base_tenv : Types.t Symbol.Table.t
