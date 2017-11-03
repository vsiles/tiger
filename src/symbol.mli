type t

val mk: string -> t
val name : t -> string

module Table : Map.S with type key = t
