(* standard library signature *)
open Core

module type Stdlib = sig
    type t
    type elt = t Symbol.Table.t
    val init: elt -> elt
  end

module Make (T: Translate.Translate) : (Stdlib with type t := Env.Make(T).entry) = struct

  module E = Env.Make(T)

    type t = E.entry
    type elt = t Symbol.Table.t

    let new_std_level name args retty =
      let label = Temp.namedlabel name in
      let level = T.newLevel ~parent:T.outermost ~name:label
          ~formals:(List.map args ~f:(fun _ -> false)) in
      E.FunEntry (level, label, args, retty)
    ;;

    (* function print(s: string) *)
    let print = new_std_level "print" [Types.String] Types.Unit;;

    (* function flush() *)
    let flush = new_std_level "flush" [] Types.Unit;;

    (* function getchar() : string *)
    let getchar = new_std_level "getchar" [] Types.String;;

    (* function ord(s: string): int *)
    let ord = new_std_level "ord" [Types.String] Types.Int;;

    (* function chr(i: int) : string *)
    let chr = new_std_level "chr" [Types.Int] Types.String;;

    (* function size(s: sintrg) : int *)
    let size = new_std_level "size" [Types.String] Types.Int;;

    (* function substring(s: string, first:int, n:int) : string *)
    let substring = new_std_level "substring" [Types.String; Types.Int; Types.Int] Types.String;;

    (* function concat (s1: string, s2: string) : string *)
    let concat = new_std_level "concat" [Types.String; Types.String] Types.String;;

    (* function not (i: int) : int *)
    let notb = new_std_level "notb" [Types.Int] Types.Int;;

    (* function exit (i: int) *)
    let exit = new_std_level "exit" [Types.Int] Types.Unit;;

    let stdlib_funs = [
        ("print", print);
        ("flush", flush);
        ("getchar", getchar);
        ("ord", ord);
        ("chr", chr);
        ("size", size);
        ("substring", substring);
        ("concat", concat);
        ("not", notb);
        ("exit", exit) ]
    ;;

    let init env = List.fold_left stdlib_funs
        ~f:(fun acc (fname, fentry) ->
            Symbol.Table.set acc ~key:(Symbol.mk fname) ~data:fentry)
        ~init:env
    ;;
    end
