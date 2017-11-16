open Core.Std
open Errors

module L = Location
module TS = Tig_syntax
module S = Syntax

type venv = Env.entry Symbol.Table.t
type tenv = Types.t Symbol.Table.t

let env_find env_name sym env =
  try
    Symbol.Table.find sym.L.item env
  with
    Not_found ->
    name_error sym.L.loc @@
    sprintf "Unknown %s: %s"
      env_name
      (Symbol.name sym.L.item)

(* Symbol.Table.t -> venv -> Env.entry *)
let venv_find = env_find "value"
(* Symbol.Table.t -> venv -> Env.entry *)
let fenv_find = env_find "function"
(* Symbol.Table.t -> tenv -> Types.t *)
let tenv_find = env_find "type"

type expty = {exp: Translate.exp; ty: Types.t}

(*
    transTy: tenv * TS.ty -> Types.t
    Might return Types.Name
*)
let transTy tenv = function
    | TS.TyName sl -> tenv_find sl tenv
    | TS.TyArray sl -> Types.Array (tenv_find sl tenv, Types.new_tag ())
    | TS.TyRecord l -> Types.Record (
        List.map l (fun f -> (f.TS.field_name.L.item, tenv_find f.TS.field_type tenv)),
        Types.new_tag())

(* temp function *)
let lift_ty ty = { exp = (); ty = ty };;

(* TEMP until connexion to get the Core version *)
let rec lassoc x = function
    | [] -> raise Not_found
    | (y, z) :: tl -> if x = y then z else lassoc x tl
;;

(* TEMP until connexion to get the Core version *)
let rec liter2 f l1 l2 = match l1, l2 with
    | hd1 :: tl1, hd2 :: tl2 -> f hd1 hd2; liter2 f tl1 tl2
    | _, _ -> ()
;;

(* transExp: venv * tenv * S.exp -> expty *)
let transExp venv tenv exp =
    let rec check_ty ty exp =
        let ty' = (trExp exp).ty in
        if ty <> ty'
        then
            type_error exp.L.loc @@
            sprintf "%s expected, found %s"
                    (Types.to_string ty) (Types.to_string ty')

    and check_int exp = check_ty Types.Int exp

    and check_unit exp = check_ty Types.Unit exp

    and trExp exp = match exp.L.item with
        | S.Lvalue vl -> trLValue vl
        | S.Nil _ -> lift_ty Types.Unit
        (*
            - type of a sequence is the type of the last entry.
            - all entries must typecheck
            - if any entry but the last is of non unit type,
              its value will be discarded
        *)
        | S.Seq ell -> List.fold_left ell
            ~f:(fun _ el -> trExp el) ~init:{ exp = (); ty = Types.Unit }
        | S.Int _ -> lift_ty Types.Int
        | S.String _ -> lift_ty Types.String
        | S.FunCall (sl, ell) -> (
            let (argsty, retty) = (match fenv_find sl venv with
                | Env.FunEntry (tylist, ty) ->
                    (List.map tylist Types.unroll, Types.unroll ty)
                | Env.VarEntry _ -> 
                type_error sl.L.loc @@
                sprintf "%s is a variable, expected a function" (Symbol.name sl.L.item)
            ) in (liter2 check_ty argsty ell; lift_ty retty)
        )
        (* FIXME handle comparison (a = b, a != b, a = nil, b != nil, nil = nil, nil != nil *)
        | S.BinOp (el1, _, el2) -> ( check_int el1; check_int el2; lift_ty Types.Int)
(*
    | Record of Symbol.t Location.loc * (Symbol.t Location.loc * exp Location.loc) list
    | Array of Symbol.t Location.loc * exp Location.loc * exp Location.loc
*)
        (* TODO improve error message here *)
        | S.If (testl, thenl, oelsel) -> (
            let thentyexp = trExp thenl in
            check_int testl;
            begin match oelsel with
                | None -> ()
                | Some elsel -> (check_ty thentyexp.ty elsel)
            end; thentyexp
        )
        | S.While (condl, bodyl) -> (
            check_int condl;
            check_unit bodyl;
            lift_ty Types.Unit
        )
        | S.For (_, froml, tol, bodyl) -> (
            check_int froml;
            check_int tol;
            check_unit bodyl;
            lift_ty Types.Unit
        )
        | S.Break _ -> lift_ty Types.Unit
        (* FIXME: handle v := nil for record *)
        | S.Assign (vl, el) -> (
            let vartyexp = trLValue vl in
            check_ty vartyexp.ty el;
            vartyexp
        )
(*
    | Let of dec list * exp Location.loc
*)
    | _ -> failwith "Todo"

    (*
        trLValue: S.lvalue location -> expty
        returns an unrolled Types.t
    *)
    and trLValue var =
    match var.L.item with
    | S.VarId sl -> lift_ty (match venv_find sl venv with
        | Env.VarEntry ty -> Types.unroll ty
        | Env.FunEntry _ ->
          type_error sl.L.loc @@
          sprintf "%s is a function, expected a variable" (Symbol.name sl.L.item)
        )
    | S.FieldAccess (vl, sl) -> (
        let ty = (trLValue vl).ty in
        match ty with
            | Types.Record (fields, _) -> (
                try lift_ty (Types.unroll (lassoc sl.L.item fields))
                with Not_found -> name_error sl.L.loc @@
                                    sprintf "Unknown field %s for record %s"
                                    (Symbol.name sl.L.item) (Types.to_string ty)
            )
            | _ -> type_error vl.L.loc @@
                    sprintf "%s is not of Record type" (Types.to_string ty)
        )
    | S.ArrayAccess (vl, el) -> (
        let ty = (trLValue vl).ty in
        match ty with
            | Types.Array (typ, _) -> (
                check_int el;
                lift_ty (Types.unroll typ)
            )
            | _ -> type_error vl.L.loc @@
                    sprintf"%s is not of Array type" (Types.to_string ty)
    )
in trExp exp
;;
(* transDec: venv * tenv * S.dec -> {venv: venv, tenv: tenv} *)
