open Core.Std

(* command line spec *)
let cmdline_spec =
    let open Command.Spec in
    empty
    +> anon ("filename" %:string)
;;

let process filename =
    printf "Input filename is %s\n" filename;
    Simple_type.print_stm Simple_type.test_prog;
    print_newline ();
    printf "max print: %d\n" (Simple_type.maxargs Simple_type.test_prog);
    print_newline ();
    Simple_type.interp Simple_type.test_prog
;;

let command =
    Command.basic
    ~summary:"Compiler for the Tiger language"
    ~readme:(fun () -> "TODO")
    cmdline_spec
    (fun filename() -> process filename)
;;

let _ =
    Command.run ~version:"1.0alph" ~build_info:"vinz" command
;;
