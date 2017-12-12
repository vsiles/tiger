(* standard library signature *)
open Core.Std

(* function print(s: string) *)
let print = Env.FunEntry (Translate.outermost,
    Temp.namedlabel "std::print", [Types.String], Types.Unit);;

(* function flush() *)
let flush = Env.FunEntry (Translate.outermost,
    Temp.namedlabel "std::flush", [], Types.Unit);;

(* function getchar() : string *)
let getchar = Env.FunEntry (Translate.outermost,
    Temp.namedlabel "std::getchar", [], Types.String);;

(* function ord(s: string): int *)
let ord = Env.FunEntry (Translate.outermost,
    Temp.namedlabel "std::ord", [Types.String], Types.Int);;

(* function chr(i: int) : string *)
let chr = Env.FunEntry (Translate.outermost,
    Temp.namedlabel "std::chr", [Types.Int], Types.String);;

(* function size(s: sintrg) : int *)
let size = Env.FunEntry (Translate.outermost,
    Temp.namedlabel "std::size", [Types.String], Types.Int);;

(* function substring(s: string, first:int, n:int) : string *)
let substring = Env.FunEntry (Translate.outermost,
    Temp.namedlabel "std::substring",
    [Types.String; Types.Int; Types.Int], Types.String);;

(* function concat (s1: string, s2: string) : string *)
let concat = Env.FunEntry (Translate.outermost,
    Temp.namedlabel "std::concat",
    [Types.String; Types.String], Types.String);;

(* function not (i: int) : int *)
let notb = Env.FunEntry (Translate.outermost,
    Temp.namedlabel "std::notb", [Types.Int], Types.Int);;

(* function exit (i: int) *)
let exit = Env.FunEntry (Translate.outermost,
    Temp.namedlabel "std::exit", [Types.Int], Types.Unit);;

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
        Symbol.Table.add acc ~key:(Symbol.mk fname) ~data:fentry)
    ~init:env
;;
