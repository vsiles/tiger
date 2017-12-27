module type Translate =
    sig
      type exp =
        | Ex of Tree.exp (* expression *)
        | Nx of Tree.stm (* void expression *)
        | Cx of (Temp.label -> Temp.label -> Tree.stm) (* conditionals *)

      type level
      type access

      val outermost: level
      val newLevel: parent:level -> name:Temp.label ->
        formals:bool list -> level
      val formals: level -> access list
      val allocLocal: level -> bool -> access
    end

module Make (F: Frame.Frame) : Translate
