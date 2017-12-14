module type Semant = sig
  val transProg: Syntax.exp -> unit
end

module Make (T: Translate.Translate) : Semant
