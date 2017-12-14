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
    (Env with type t := T.access and type t' := T.level)
