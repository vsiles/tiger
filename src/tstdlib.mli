module type Stdlib = sig
    type t
    type elt = t Symbol.Table.t
    val init: elt -> elt
  end

module Make (T: Translate.Translate) : (Stdlib with type t := Env.Make(T).entry)
