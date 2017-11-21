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

(* Helper function to extract type info for function signature *)
let trans_fun_sig tenv lfundec =
  let fundec = lfundec.L.item in
  let argsty = List.map fundec.S.args
    (fun field -> field.S.field_name, tenv_find field.S.field_type tenv) in
  let retty = match fundec.S.return_type with
    | None -> Types.Unit
    | Some ret -> tenv_find ret tenv in
  argsty, retty
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
                | Env.FunEntry (tylist, ty) -> (* might need to unroll *)
                  (tylist, ty)
                | Env.VarEntry _ ->
                type_error sl.L.loc @@
                sprintf "%s is a variable, expected a function" (Symbol.name sl.L.item)
            ) in (List.iter2_exn argsty ell check_ty; lift_ty retty)
        )
        | S.BinOp (el1, opl, el2) -> (
            let expty1 = trExp el1 in
            let expty2 = trExp el2 in
            match opl.L.item with
            | S.Eq | S.Neq -> begin
                (* first, check for nil vs nil *)
                match el1.L.item, el2.L.item with
                | S.Nil _, S.Nil _ ->
                  type_error exp.L.loc
                    "Spotted equality test between two untyped 'nil', which is forbidden"
                | _, S.Nil _ -> (match expty1.ty with
                    | Types.Record _ -> lift_ty Types.Int (* bool is Types.Int *)
                    | _ -> type_error exp.L.loc @@ sprintf
                        "Equality test between 'nil' and non record expression (of type %s)"
                        (Types.to_string expty1.ty)
                  )
                | S.Nil _, _ -> (match expty2.ty with
                    | Types.Record _ -> lift_ty Types.Int (* bool is Types.Int *)
                    | _ -> type_error exp.L.loc @@ sprintf
                        "Equality test between 'nil' and non record expression (of type %s)"
                        (Types.to_string expty2.ty)
                  )
                | _, _ -> if expty1.ty <> expty2.ty then
                    type_error exp.L.loc @@ sprintf
                      "Ill-typed equality test: %s vs %s"
                      (Types.to_string expty1.ty) (Types.to_string expty2.ty)
                  else lift_ty Types.Int
              end
            | _ -> (check_int el1; check_int el2; lift_ty Types.Int)
          )
        | S.Record (sl, fl) -> (
            (* sort field def to easily compare with sorted Record def *)
            let sorted_fl = List.sort ~cmp:field_loc_cmp fl in
            let recty = tenv_find sl tenv in begin
            match recty with
            | Types.Record (fields, _) -> begin
                List.iter2_exn fields sorted_fl (fun (fname, ftyp) (name, body) ->
                    if Symbol.equal fname name.L.item then
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
                if init_tyexp.ty = ty then lift_ty arrty
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
        (* TODO: think of a way to prevent assignation to sym inside bodyl *)
        | S.For (sym, froml, tol, bodyl) -> (
            let venv' = Symbol.Table.add sym (Env.VarEntry Types.Int) venv in
            let ty = (transExp  venv' tenv bodyl).ty in
            if ty <> Types.Unit
            then type_error exp.L.loc @@
                sprintf "The body of a For loop must be of Unit type, found %s"
                (Types.to_string ty)
            else (check_int froml; check_int tol; lift_ty Types.Unit)
          )
        (* TODO: check that BREAK is inside a loop *)
        | S.Break _ -> lift_ty Types.Unit
        | S.Assign (vl, el) -> (
            let ret = lift_ty Types.Unit in
            let varty = (trLValue vl).ty in
            let unrolled_type = Types.unroll varty in
            match unrolled_type, el.L.item with
                | Types.Record _, S.Nil _ -> ret
                | _, _ -> (check_ty varty el; ret)
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
          let styp = Types.unroll (tenv_find sl tenv) in begin
              match styp, var.S.value.L.item with
              | Types.Record _, S.Nil _ -> () (* var a : foo := nil allowed if foo is a Record *)
              | _ , _ -> if styp <> tyexp.ty then
                  type_error vl.L.loc @@
                  sprintf "%s is incompatible with %s"
                  (Types.to_string styp) (Types.to_string tyexp.ty)
          end
        | None -> (* var a := nil is forbidden *) begin
            match var.S.value.L.item with
            | S.Nil _ -> type_error vl.L.loc
                "Can't initialize a variable using 'nil' without an explicit type"
            | _ -> ()
        end
      end;
      Symbol.Table.add var.S.var_name.L.item (Env.VarEntry tyexp.ty) venv, tenv
    )
  | S.FunDec funlist ->
    (* gather the headers of each function *)
    let venv' =
        List.fold_left
            funlist
            ~f:(fun venv_acc lfundec -> let argsty,retty = trans_fun_sig tenv lfundec in
                printf "Adding fun %s to env\n" (Symbol.name lfundec.L.item.S.fun_name.L.item);
                Symbol.Table.add
                    lfundec.L.item.S.fun_name.L.item
                    (Env.FunEntry (List.map argsty snd, retty))
                    venv_acc)
            ~init:venv in
    (* then parse each body with all headers in the environment *)
    List.fold_left funlist
      ~f:(fun venv_acc fundec -> trans_fun venv_acc tenv fundec)
      ~init:venv', tenv
  | S.TypeDec typlist ->
    venv, List.fold_left typlist
      ~f:(fun tenv_acc typdec -> trans_typ tenv_acc typdec)
      ~init:tenv

and trans_typ tenv ltypdec =
  let typdec = ltypdec.L.item in
  let type_name = typdec.S.type_name in
  let typ = typdec.S.typ in
  Symbol.Table.add type_name.L.item (transTy tenv typ) tenv

  (* TODO: optimize to avoid recomputing trans_fun_sig here *)
and trans_fun venv tenv lfundec =
  let fundec = lfundec.L.item in
  let argsty,retty = trans_fun_sig tenv lfundec in
  let venv' = List.fold_left argsty
      ~f:(fun venv_acc (name, ty) -> Symbol.Table.add name.L.item
             (Env.VarEntry ty) venv_acc)
      ~init:venv in
  let body_tyexp = transExp venv' tenv fundec.S.body in
  if body_tyexp.ty <> retty then
      type_error lfundec.L.loc @@
      sprintf "The body of this function is of type %s, not %s"
        (Types.to_string body_tyexp.ty) (Types.to_string retty)
  else venv'
;;

(* transProg: Syntax.exp -> unit *)
let transProg exp =
  let lexp = L.mkdummy exp in
  let venv = Stdlib.init Env.base_venv in
  let _ = transExp venv Env.base_tenv lexp in ()
