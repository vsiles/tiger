module type Frame =
    sig
        type frame
        type access

        val newFrame: name:Temp.label -> formals:bool list -> frame
        val name: frame -> Temp.label
        val formals: frame -> access list
        val allocLocal: frame -> bool -> access
        val externalCall : string -> Tree.exp list -> Tree.exp

        val fp: Temp.temp
        val wordSize: int
        val exp: access -> Tree.exp -> Tree.exp
    end
