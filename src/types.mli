type tag

val new_tag : unit -> tag

type t =
    | Int
    | String
    | Record of (Symbol.t * t) list * tag
    | Array of t * tag
    | Nil
    | Unit
    | Name of Symbol.t * t option ref

val compat: t -> t -> bool
val eq_compat: t -> t -> bool

val to_string : t -> string
