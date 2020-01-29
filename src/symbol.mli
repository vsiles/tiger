open Core

type t

val mk: string -> t
val name : t -> string
val equal : t -> t -> int

module Table : Map.S with type Key.t = t
