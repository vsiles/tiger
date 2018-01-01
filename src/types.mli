type tag

val new_tag : unit -> tag

type t =
    | Int
    | String
    | Record of record
    | Array of t * tag
    | Nil
    | Unit
    | Name of Symbol.t * t option ref
and record = {
  fields: (Symbol.t * t) list;
  tag: tag
}

val compat: t -> t -> bool
val eq_compat: t -> t -> bool

val to_string : t -> string
