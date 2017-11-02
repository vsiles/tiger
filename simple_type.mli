type stm
type exp

val test_prog: stm
val print_stm: stm -> unit

val maxargs: stm -> int
val interp: stm -> unit
