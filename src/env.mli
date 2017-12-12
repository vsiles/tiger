type entry =
    | VarEntry of Translate.access * Types.t * bool
    | FunEntry of Translate.level * Temp.label * Types.t list * Types.t

val base_venv : entry Symbol.Table.t
val base_tenv : Types.t Symbol.Table.t
