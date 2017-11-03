type entry =
    | VarEntry of Types.t
    | FunEntry of Types.t list * Types.t

val base_venv : Types.t Symbol.Table.t
val base_tenv : entry Symbol.Table.t
