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

      val placeholder: exp

(*      val pp_access: access -> unit *)
    end

module Make (F: Frame.Frame) : Translate
