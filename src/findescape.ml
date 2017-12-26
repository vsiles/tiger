open Core.Std
open Errors

module L = Location
module S = Syntax

type depth = int

type escEnv = (depth * bool ref) Symbol.Table.t

let env_find sym env =
  match Symbol.Table.find env sym.L.item with
  | Some x -> x
  | None ->
    name_error sym.L.loc @@
    sprintf "Unknown variable: %s" (Symbol.name sym.L.item)
;;

(* escEnv -> depth -> Syntax.exp L.loc -> unit *)
let rec traverseExp env depth exp =
  (* Syntax.exp -> unit *)
    let rec traverse exp = match exp.L.item with
      | S.Lvalue vl -> traverseVar vl
      | S.Nil _ -> ()
      | S.Seq ell -> List.iter ell ~f:traverse
      | S.Int _ -> ()
      | S.String _ -> ()
      | S.FunCall (_, ell) -> List.iter ell ~f:traverse
      | S.BinOp (el1, _, el2) -> begin traverse el1; traverse el2; end
      | S.Record (_, fl) -> List.iter fl ~f:(fun (_, el) -> traverse el)
      | S.Array (sl, sizel, initl) -> begin traverse sizel; traverse initl; end
      | S.If (testl, thenl, oelsel) -> begin
          traverse testl;
          traverse thenl;
          match oelsel with
          | None -> ()
          | Some elsel -> traverse elsel
        end
      | S.While (condl, bodyl) -> begin traverse condl; traverse bodyl; end
      | S.For (sym, esc, froml, tol, bodyl) ->
          let _ = printf "FindEscape: For loop at depth %d\n" depth in
          let env' = Symbol.Table.add env ~key:sym ~data:(depth, esc) in
            traverse froml;
            traverse tol;
            traverseExp env' depth tol
      | S.Break _ -> ()
      | S.Assign (vl, el) -> begin traverseVar vl; traverse el; end
      | S.Let (decl, el) ->
          let env' = traverseDecs env depth decl in
          traverseExp env' depth el
    (* traverseVar: S.lvalue location -> unit *)
    and traverseVar var = match var.L.item with
      | S.VarId sl ->
        let (decl_depth, esc) = env_find sl env in
        if decl_depth < depth then (
          printf "Flipping %s at level %d (was %d)\n"
                 (Symbol.name sl.L.item)
                 depth decl_depth;
        esc :=  true ) else ()
      | S.FieldAccess (vl, _) -> traverseVar vl
      | S.ArrayAccess (vl, el) -> begin
          traverseVar vl;
          traverse el;
        end
    in traverse exp

(* escEnv -> depth -> S.dec list -> escEnv *)
and traverseDecs env depth l =
  (* escEnv -> depth -> S.dec -> escEnv *)
  let traverseDec env = function
    | S.TypeDec _ -> env
    | S.VarDec vl -> let var = vl.L.item in
      let var_name = var.S.var_name.L.item
      and var_escape = var.S.escape
      and value = var.S.value in begin
        traverseExp env depth value;
        Symbol.Table.add env var_name (depth, var_escape)
      end
    | S.FunDec fundecs ->
      let newDepth name escape env =
        Symbol.Table.add env ~key:name ~data:(depth + 1, escape)
      in let traverseFunDec args body =
           let env' = List.fold_right args
               ~f:(fun field acc -> let fname = field.S.field_name.L.item
                   and escp = field.S.escape in newDepth fname escp acc)
               ~init:env
           in traverseExp env' (depth + 1) body
      in begin
        List.iter fundecs ~f:(fun fdl -> let fd = fdl.L.item in
                               traverseFunDec fd.S.args fd.S.body);
        env
        end
  in List.fold_left l ~f:(fun acc dec -> traverseDec acc dec) ~init:env

(* Syntax.exp -> unit *)
let findEscape exp =
  let lexp = L.mkdummy exp in
  traverseExp Symbol.Table.empty 0 lexp
;;
