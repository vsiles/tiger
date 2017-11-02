open Core.Std
open Core.Std.Option

type id = string

type binop = Plus | Minus | Times | Div

type stm = CompoundStm of stm * stm
         | AssignStm of id * exp
         | PrintStm of exp list

    and exp = IdExp of id
            | NumExp of int
            | OpExp of exp * binop * exp
            | EseqExp of stm * exp

let test_a = AssignStm ("a", OpExp (NumExp 5, Plus, NumExp 3))
let test_print = PrintStm [ IdExp "a"; OpExp (IdExp "a", Minus, NumExp 1)]
let test_b = AssignStm ("b", EseqExp (test_print, OpExp (NumExp 10, Times, IdExp "a")))

let test_prog = CompoundStm (test_a, CompoundStm (test_b, PrintStm [IdExp "b"]))

let string_of_binop = function
    | Plus -> " + "
    | Minus -> " - "
    | Times -> " * "
    | Div -> " \ "
;;

let rec print_stm = function
    | CompoundStm (s1, s2) -> (
        print_stm s1;
        printf "%s" "\n";
        print_stm s2
    )
    | AssignStm (id, exp) -> (
        printf "%s := " id;
        print_exp exp;
    )
    | PrintStm [] -> printf "%s" "[]"
    | PrintStm [ x ] -> (
        printf "%s" "[ ";
        print_exp x;
        printf "%s" " ]"
    )
    | PrintStm (hd :: tl) -> (
        printf "%s" "[ ";
        print_exp hd;
        List.iter tl (fun e -> printf "%s" ", "; print_exp e);
        printf "%s" " ]"
    )
and print_exp = function
    | IdExp id -> printf "%s" id
    | NumExp n -> printf "%d" n
    | OpExp (e1, op, e2) -> (
        print_exp e1;
        printf "%s" (string_of_binop op);
        print_exp e2
    )
    | EseqExp (s, e) -> (
        print_stm s;
        printf "%s" "\n";
        print_exp e
    )
;;

let rec maxargs = function
    | CompoundStm (s1, s2) -> max (maxargs s1) (maxargs s2)
    | AssignStm (_, exp) -> maxargs_exp exp
    | PrintStm l ->
        List.fold_left l ~f:(fun acc elem -> acc + (maxargs_exp elem)) ~init:(List.length l)
and maxargs_exp = function
    | IdExp _ -> 0
    | NumExp _ -> 0
    | OpExp (e1, _, e2) -> (maxargs_exp e1) + (maxargs_exp e2)
    | EseqExp (s, e) -> (maxargs s) + (maxargs_exp e)
;;

type table = (string * int) list

let rec lookup id = function
    | [] -> None
    | ((x, e) :: tl) -> if x = id then Some e else lookup id tl
;;

let interpOp x y = function
    | Plus -> x + y
    | Minus -> x - y
    | Times -> x * y
    | Div -> x / y
;;

let rec interpStm env = function
    | CompoundStm (s1, s2) -> (
        bind (interpStm env s1) (fun new_env ->
            interpStm new_env s2)
    )
    | AssignStm (id, e) ->
            bind (interpExp env e) (fun (n, _) ->
                return ((id, n) :: env)
            )
    | PrintStm l -> (let ret = interpExpList env l in  print_newline (); ret)
and interpExp env = function
    | IdExp id -> bind (lookup id env) (fun n -> return (n, env))
    | NumExp n -> return (n, env)
    | OpExp (e1, op, e2) ->
            bind (interpExp env e1) (fun (n1, _) ->
                bind (interpExp env e2) (fun (n2, _) ->
                    return (interpOp n1 n2 op, env)
                )
            )
    | EseqExp (s, e) -> bind (interpStm env s) (fun new_env -> interpExp new_env e)
and interpExpList env = function
    | [] -> return env;
    | hd :: tl -> bind (interpExp env hd) (fun (n, new_env) ->
            printf "%d " n;
            interpExpList new_env tl
    )
;;

let interp stm = match (interpStm [] stm) with
    | Some _ -> ()
    | None -> failwith "Error during interp"
