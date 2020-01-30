open Core
open OUnit2

module T = Translate.Make(Arm32frame.ARM32Frame)
module S = Semant.Make(T)

let process filename =
  try
    let inx = In_channel.create filename in
    let lexbuf = Lexing.from_channel inx in
    let raw_term = Parser.prog Lexer.lexer lexbuf in
    let expr = Syntax.translate raw_term in
    let _ = S.transProg expr in
    In_channel.close inx;
    0
  with
  | Errors.TError (_e, _l, _m) -> -1
  | Sys_error _s -> -2
;;

let printer n =
  if n = 0 then "OK"
  else if n = -1 then "Compilation Failed"
  else if n = -2 then "System Error"
  else "Unknown Error"
;;

let test_good filename _test_ctxt =
  assert_equal ~printer 0 (process @@ sprintf "./good/%s" filename)
;;

let test_bad filename _test_ctxt =
  assert_equal ~printer (-1) (process @@ sprintf "./bad/%s" filename)
;;
(*
let test_queens test_ctxt = test_good test_ctxt "good/queens.tig";;

let suite =
    "suite">:::
        ["queens">:: test_queens]
;;
*)

let filter_tig name = Filename.check_suffix name ".tig";;

let () =
    let good_tests = Array.to_list (Sys.readdir "./good/") in
    let bad_tests = Array.to_list (Sys.readdir "./bad/") in
    let good_tigs = List.filter good_tests ~f:filter_tig in
    let bad_tigs = List.filter bad_tests ~f:filter_tig in
    let tests1 = List.fold_left
        good_tigs
        ~f:(fun tl hd -> (hd>:: (test_good hd)) :: tl)
        ~init:[] in
    let tests2 = List.fold_left
        bad_tigs
        ~f:(fun tl hd -> (hd>:: (test_bad hd)) :: tl)
        ~init:tests1 in
    let suite = "full suite">::: tests2 in
    run_test_tt_main suite
;;
