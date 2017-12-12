open Core.Std

type temp

val newtemp: unit -> temp
val makestring: temp -> string
(*module Table : Map.S with type Key.t = temp*)

type label = Symbol.t
val newlabel: unit -> label
val namedlabel: string -> label
