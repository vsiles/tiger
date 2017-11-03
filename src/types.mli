type tag

val create : unit -> tag

type t =
    | Int
    | String
    | Record of (Symbol.t * t) list * tag
    | Array of t * tag
    | Nil
    | Unit
    | Name of Symbol.t * t option ref

(* extract the actual 't' of a Name type *)
val unroll: t -> t

val to_string : t -> string
