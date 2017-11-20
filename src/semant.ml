open Core.Std
open Errors

module L = Location
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
    transTy: tenv -> S.ty -> Types.t
    Might return Types.Name
*)
let transTy tenv = function
    | S.TyName sl -> tenv_find sl tenv
    | S.TyArray sl -> Types.Array (tenv_find sl tenv, Types.new_tag ())
    | S.TyRecord l -> Types.Record (
        List.map l (fun f -> (f.S.field_name.L.item, tenv_find f.S.field_type tenv)),
        Types.new_tag())
;;

(* temp function *)
let lift_ty ty = { exp = (); ty = ty };;

(* Compare Record fields declaration (annoted by Location)
   to be used to sort them during type checking

   To be used with Symbol.t Location.loc * exp Location.loc) list
*)
let field_loc_cmp field_def1 field_def2 =
  let name1 = fst field_def1 in
  let name2 = fst field_def2 in
  Pervasives.compare
    (Symbol.name name1.L.item)
    (Symbol.name name2.L.item)
;;

(* TODO improve error message here,
   might remove some calls to check_* to have more precise log *)
(* transExp: venv -> tenv -> S.exp -> expty *)
let rec transExp venv tenv exp =
    let rec check_ty ty exp =
        let ty' = (trExp exp).ty in
        if not (phys_equal ty ty')
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
                | Env.FunEntry (tylist, ty) -> (* might need to unroll *)
                  (tylist, ty)
                | Env.VarEntry _ ->
                type_error sl.L.loc @@
                sprintf "%s is a variable, expected a function" (Symbol.name sl.L.item)
            ) in (List.iter2_exn argsty ell check_ty; lift_ty retty)
        )
        (* FIXME handle comparison (a = b, a != b, a = nil, b != nil, nil = nil, nil != nil *)
        | S.BinOp (el1, _, el2) -> (check_int el1; check_int el2; lift_ty Types.Int)
        | S.Record (sl, fl) -> (
            (* sort field def to easily compare with sorted Record def *)
            let sorted_fl = List.sort ~cmp:field_loc_cmp fl in
            let recty = tenv_find sl tenv in begin
            match recty with
            | Types.Record (fields, _) -> begin
                List.iter2_exn fields sorted_fl (fun (fname, ftyp) (name, body) ->
                    if phys_equal fname name.L.item then
                      check_ty ftyp body
                    else
                      name_error name.L.loc
                      @@ sprintf "Wrong field %s: expected %s"
                        (Symbol.name name.L.item) (Symbol.name fname))
              end
            | _ -> type_error sl.L.loc @@
              sprintf "%s is not of Record type" (Symbol.name sl.L.item)
          end; lift_ty recty
          )
        | S.Array (sl, sizel, initl) -> (
            check_int sizel;
            let init_tyexp = trExp initl in
            let arrty = tenv_find sl tenv in
            begin match arrty with
              | Types.Array (ty, _) ->
                if phys_equal init_tyexp.ty ty then lift_ty arrty
                else
                  type_error initl.L.loc @@
                  sprintf
                    "Wrong type for initial value of an array: found %s, expected %s"
                    (Types.to_string init_tyexp.ty) (Types.to_string ty)
              | _ ->
                type_error sl.L.loc @@
                sprintf "Not an array type: %s" (Types.to_string arrty)
            end
          )
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
        (* TODO: check that BREAK is inside a loop *)
        | S.Break _ -> lift_ty Types.Unit
        (* FIXME: handle v := nil for record *)
        | S.Assign (vl, el) -> (
            let vartyexp = trLValue vl in
            check_ty vartyexp.ty el;
            vartyexp
        )
        | S.Let (decl, el) ->
            let venv', tenv' = transDecs venv tenv decl in
            transExp venv' tenv' el

    (* trLValue: S.lvalue location -> expty *)
    and trLValue var =
    match var.L.item with
    | S.VarId sl -> lift_ty (match venv_find sl venv with
        | Env.VarEntry ty -> ty (* might need unroll *)
        | Env.FunEntry _ ->
          type_error sl.L.loc @@
          sprintf "%s is a function, expected a variable" (Symbol.name sl.L.item)
        )
    | S.FieldAccess (vl, sl) -> (
        let ty = (trLValue vl).ty in
        match ty with
            | Types.Record (fields, _) -> (
                try lift_ty (List.Assoc.find_exn fields sl.L.item) (* might need unroll *)
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
                lift_ty typ (* might need unroll *)
            )
            | _ -> type_error vl.L.loc @@
                    sprintf "%s is not of Array type" (Types.to_string ty)
    )
in trExp exp

and transDecs venv tenv =
  List.fold_left ~f:(fun (venv', tenv') dec -> transDec venv' tenv' dec)
    ~init:(venv, tenv)

(* /!\ First attempt, don't support mutual types/functions *)
(* transDec: venv -> tenv -> S.dec -> (venv * tenv) *)
and transDec venv tenv = function
  | S.VarDec vl -> (
      let var = vl.L.item in
      let tyexp = transExp venv tenv var.S.value in
      begin
        match var.S.var_type with
        | Some sl ->
          let styp = tenv_find sl tenv (* might need unroll *) in
          if not (phys_equal styp tyexp.ty) then
            type_error vl.L.loc @@
            sprintf "%s is incompatible with %s" (Types.to_string styp)
              (Types.to_string tyexp.ty)
        | None -> ()
      end;
      Symbol.Table.add var.S.var_name.L.item (Env.VarEntry tyexp.ty) venv, tenv
    )
  | S.FunDec funlist ->
    List.fold_left funlist
      ~f:(fun venv_acc fundec -> trans_fun venv_acc tenv fundec)
      ~init:venv, tenv
  | S.TypeDec typlist ->
    venv, List.fold_left typlist
      ~f:(fun tenv_acc typdec -> trans_typ tenv_acc typdec)
      ~init:tenv

and trans_typ tenv ltypdec =
  let typdec = ltypdec.L.item in
  let type_name = typdec.S.type_name in
  let typ = typdec.S.typ in
  Symbol.Table.add type_name.L.item (transTy tenv typ) tenv

and trans_fun venv tenv lfundec =
  let fundec = lfundec.L.item in
  let argsty = List.map fundec.S.args
    (fun field -> field.S.field_name, tenv_find field.S.field_type tenv) in
  let retty = match fundec.S.return_type with
    | None -> Types.Unit
    | Some ret -> tenv_find ret tenv in
  let fentry = Env.FunEntry (List.map argsty snd, retty) in
  let venv' = List.fold_left argsty
      ~f:(fun venv_acc (name, ty) -> Symbol.Table.add name.L.item
             (Env.VarEntry ty) venv_acc)
      ~init:venv in
  let venv'' = Symbol.Table.add fundec.S.fun_name.L.item fentry venv' in
  let body_tyexp = transExp venv'' tenv fundec.S.body in
  if not (phys_equal body_tyexp.ty retty) then
      type_error lfundec.L.loc @@
      sprintf "The body of this function is of type %s, not %s"
        (Types.to_string body_tyexp.ty) (Types.to_string retty)
  else venv''
;;

(* transProg: Syntax.exp -> unit *)
let transProg exp =
  let lexp = L.mkdummy exp in
  let _ = transExp Env.base_venv Env.base_tenv lexp in ()
