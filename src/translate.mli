module type Translate =
    sig
      type exp
      type level
      type access

      val outermost: level
      val newLevel: parent:level -> name:Temp.label ->
        formals:bool list -> level
      val formals: level -> access list
      val allocLocal: level -> bool -> access

      val simpleVar: access -> level -> exp
      val arrayAccess: exp -> exp -> exp
      val fieldAccess: exp -> Symbol.t -> Symbol.t list -> exp
      val intConst: int -> exp
      val binOperation : Syntax.op -> exp -> exp -> exp
      val nilExp: exp
      val unitExp: exp
      val ifthenelse : exp -> exp -> exp -> exp
      val stringExp : string -> exp
      val recordExp: exp list -> exp
      val arrayExp: exp -> exp -> exp
      val whileExp: exp -> exp -> Temp.label -> exp
      val breakExp: Temp.label -> exp
      val forExp: exp -> exp -> exp -> exp -> Temp.label -> exp
      val callExp: Temp.label -> exp list -> level -> level -> exp

      val placeholder: exp

(*      val pp_access: access -> unit *)
    end

module Make (F: Frame.Frame) : Translate
