module type Env = sig
  type t
  type t'
  type entry =
    (* The bool is the assignable using := or not (e.g. for loop index) *)
    | VarEntry of t * Types.t * bool
    | FunEntry of t' * Temp.label * Types.t list * Types.t

  val base_venv : entry Symbol.Table.t
  val base_tenv : Types.t Symbol.Table.t
end

module Make (T: Translate.Translate) :
    (Env with type t := T.access and type t' := T.level) = struct

    type t = T.access
    type t' = T.level

    type entry =
        (* The bool is the assignable using := or not (e.g. for loop index) *)
        | VarEntry of t * Types.t * bool
        | FunEntry of t' * Temp.label * Types.t list * Types.t

    let base_venv = Symbol.Table.empty

    let base_tenv =
      let tempty = Symbol.Table.empty in
      let tint = Symbol.Table.set tempty ~key:(Symbol.mk "int") ~data:Types.Int in
      Symbol.Table.set tint ~key:(Symbol.mk "string") ~data:Types.String
  end
