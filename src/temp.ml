open Core.Std

type temp = int

let newtemp =
    let n = ref (-1) in
    function () ->
        incr n; !n
;;

let makestring t =
    sprintf "T%d" t
;;

type label = Symbol.t

let labelfuns =
    let l = ref (-1) in
    ((function () -> incr l; Symbol.mk (sprintf "L%d" (!l))),
     (function s -> incr l; Symbol.mk (sprintf "NL%d-%s" (!l) s)))
;;

let newlabel = fst labelfuns;;
let namedlabel = snd labelfuns;;
