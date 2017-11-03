type entry =
    | VarEntry of Types.t
    | FunEntry of Types.t list * Types.t

let base_venv = Symbol.Table.empty

let base_tenv =
    let tempty = Symbol.Table.empty in
    let tint = Symbol.Table.add (Symbol.mk "int") Types.Int tempty in
    Symbol.Table.add (Symbol.mk "string") Types.String tint
