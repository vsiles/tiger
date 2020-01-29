open Core
open Errors

(* command line spec *)
let cmdline_spec =
    let open Command.Spec in
    empty
    +> anon ("filename" %:string)
;;

module T = Translate.Make(Arm32frame.ARM32Frame)
module S = Semant.Make(T)

let process filename =
    try
        let inx = In_channel.create filename in
        let lexbuf = Lexing.from_channel inx in
        printf "Parsing: %s\n" filename;
        let raw_term = Parser.prog Lexer.lexer lexbuf in
        let expr = Syntax.translate raw_term in
        let _ = Findescape.findEscape expr in
        printf "> Parsing: OK\n";
        let _ = S.transProg expr in
        printf "> Typing: OK\n";
        In_channel.close inx
    with
        | TError (e, l, m) -> printf "Error: %s\n" (msg_of_error e l m)
        | Sys_error s -> printf "System Error: %s\n" s
;;

let command =
    Command.basic_spec
    ~summary:"Compiler for the Tiger language"
    ~readme:(fun () -> "TODO")
    cmdline_spec
    (fun filename() -> process filename)
;;

let _ =
    Command.run ~version:"1.0alph" ~build_info:"vinz" command
;;
