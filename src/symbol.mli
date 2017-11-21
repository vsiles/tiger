type t

val mk: string -> t
val name : t -> string
val equal : t -> t -> bool

module Table : Map.S with type key = t
