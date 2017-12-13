module type Env = sig
  type entry
  val base_venv : entry Symbol.Table.t
  val base_tenv : Types.t Symbol.Table.t
end

module Make (T: Translate.Translate) :
  (Env with
    type entry = [ `VarEntry of T.access * Types.t * bool
                 |`FunEntry of T.level * Temp.label * Types.t list * Types.t
                 ])
