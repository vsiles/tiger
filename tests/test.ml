open Core.Std
open OUnit2
open Errors

let process filename =
    try
        let inx = In_channel.create filename in
        let lexbuf = Lexing.from_channel inx in
        let raw_term = Parser.prog Lexer.lexer lexbuf in
        let expr = Syntax.translate raw_term in
        let _ = Semant.transProg expr in
        In_channel.close inx;
        assert_equal 1 1
    with
        | TError (e, l, m) -> assert_equal 1 0
        | Sys_error s -> assert_equal 1 0

let test_queens test_ctxt = process "tests/good/queens.tig";;

let suite =
    "suite">:::
        ["queens">:: test_queens]
;;

let () =
    run_test_tt_main suite
;;
