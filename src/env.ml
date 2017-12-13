module type Env = sig
  type entry
  val base_venv : entry Symbol.Table.t
  val base_tenv : Types.t Symbol.Table.t
end

module Make (T: Translate.Translate) :
  (Env with
    type entry = [ `VarEntry of T.access * Types.t * bool
                 |`FunEntry of T.level * Temp.label * Types.t list * Types.t
                 ]) = struct
  type entry = [
      (* The bool is the assignable using := or not (e.g. for loop index) *)
        `VarEntry of T.access * Types.t * bool
      | `FunEntry of T.level * Temp.label * Types.t list * Types.t
    ]
    let base_venv = Symbol.Table.empty

    let base_tenv =
      let tempty = Symbol.Table.empty in
      let tint = Symbol.Table.add tempty ~key:(Symbol.mk "int") ~data:Types.Int in
      Symbol.Table.add tint ~key:(Symbol.mk "string") ~data:Types.String
  end
