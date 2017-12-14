module type Translate =
    sig
        type exp = unit (* pour l'instant *)

        type level
        type access

        val outermost: level
        val newLevel: parent:level -> name:Temp.label ->
            formals:bool list -> level
        val formals: level -> access list
        val allocLocal: level -> bool -> access
    end

module Make (F: Frame.Frame) : Translate
