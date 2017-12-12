type entry =
  (* The bool is the assignable using := or not (e.g. for loop index) *)
  | VarEntry of Translate.access * Types.t * bool
  | FunEntry of Translate.level * Temp.label * Types.t list * Types.t

let base_venv = Symbol.Table.empty

let base_tenv =
  let tempty = Symbol.Table.empty in
  let tint = Symbol.Table.add tempty ~key:(Symbol.mk "int") ~data:Types.Int in
  Symbol.Table.add tint ~key:(Symbol.mk "string") ~data:Types.String
