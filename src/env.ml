type entry =
  (* is the assignable using := or not (e.g. for loop index) *)
  | VarEntry of Types.t * bool
  | FunEntry of Types.t list * Types.t

let base_venv = Symbol.Table.empty

let base_tenv =
  let tempty = Symbol.Table.empty in
  let tint = Symbol.Table.add tempty ~key:(Symbol.mk "int") ~data:Types.Int in
  Symbol.Table.add tint ~key:(Symbol.mk "string") ~data:Types.String
