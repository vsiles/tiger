open Core.Std
open Errors

module L = Location
module S = Syntax
module UF = Core_kernel.Union_find

type venv = Env.entry Symbol.Table.t
type tenv = Types.t Symbol.Table.t

let env_find env_name sym env =
  match Symbol.Table.find env sym.L.item with
  | Some x -> x
  | None ->
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
    Must not go under Types.Name in order to be able
    to deal with mutual recursive types
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

(* Check if a list of mutual type declaration has invalid cycles.
   Return true iff there is a cycle
   Assumption: no shadowing/duplicates is allowed, otherwise a situation like

   type toto = { ... }
   type tata = toto
   type toto = { ... toto ... }

   is impossible to diagnostic correctly
*)
let check_typdec_cycle ltypdec_list =
  let rec check_typdec_cycle_gen id ufs = function
    | ltypdec :: tl -> begin
        let typdec = ltypdec.L.item in
        let left = typdec.S.type_name.L.item in
        let typ = typdec.S.typ in
        match typ with
        | S.TyName lname -> let right = lname.L.item in
          let left_id, id, ufs =
            match Symbol.Table.find ufs left with
            | Some uf -> uf, id, ufs
            | None -> let new_uf = UF.create id in
              new_uf, id + 1, Symbol.Table.add ufs ~key:left ~data:new_uf
          in
          let right_id, id, ufs =
            match Symbol.Table.find ufs right with
            | Some uf -> uf, id, ufs
            | None -> let new_uf = UF.create id in
              new_uf, id + 1, Symbol.Table.add ufs ~key:right ~data:new_uf
          in
          if UF.same_class left_id right_id then
            (* Spotted a cycle *) true
          else (
            UF.union left_id right_id;
            check_typdec_cycle_gen id ufs tl
          )
        | _ -> let ufs = Symbol.Table.add ufs ~key:left ~data:(UF.create id) in
          check_typdec_cycle_gen (id + 1) ufs tl
      end
    | [] -> false
  in check_typdec_cycle_gen 0 Symbol.Table.empty ltypdec_list
;;

(* Check if a list of mutual type declaration has dups (a.k.a. shadowing)
   Return true if dups are spotted
*)
let check_typdec_shadowing ltypdec_list =
  let rec check_typdec_shadowing_gen = function
    | ltypdec :: tl -> begin
        let typdec = ltypdec.L.item in
        let type_name = typdec.S.type_name.L.item in
        if List.exists tl
          ~f:(fun x -> Symbol.equal type_name x.L.item.S.type_name.L.item)
        then true
        else check_typdec_shadowing_gen tl
      end
    | [] -> false
  in check_typdec_shadowing_gen ltypdec_list
;;

(* Replace remaining Types.Name (_, !None) with relevant !Some *)
let fix_name_ty tenv =
  let rec fix ty = match ty with
    | Types.Name (sym, opty_ref) -> begin
        match !opty_ref with
        | Some _ -> ()
        | None -> opty_ref := Some (tenv_find (L.mkdummy sym) tenv)
      end
    | Types.Array (arrty, _) -> fix arrty
    | Types.Record (fields, _) ->
      List.iter fields ~f:(fun (_, t) -> fix t)
    | _ -> ()
  in (
    Symbol.Table.iteri tenv (fun ~key ~data -> fix data);
    tenv
  )
;;

(* TODO improve error message here,
   might remove some calls to check_* to have more precise log *)
(* transExp: venv -> tenv -> S.exp -> expty *)
(* Note: tenv must not have Types.Name(None) entries *)
let rec transExp venv tenv exp =
  let rec check_ty ty exp =
    let ty' = (trExp exp).ty in
    if not @@ Types.compat ty ty'
    then type_error exp.L.loc @@
      sprintf "%s expected, found %s"
        (Types.to_string ty) (Types.to_string ty')

    and check_int exp = check_ty Types.Int exp

    and check_unit exp = check_ty Types.Unit exp

    and trExp exp = match exp.L.item with
        | S.Lvalue vl -> fst @@ trLValue vl
        | S.Nil _ -> lift_ty Types.Nil
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
                  (tylist, ty)
                | Env.VarEntry _ ->
                  type_error sl.L.loc @@
                  sprintf "%s is a variable, expected a function"
                    (Symbol.name sl.L.item)
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
                | _, _ -> if (not (Types.eq_compat expty1.ty expty2.ty)) ||
                             (not (Types.compat expty1.ty expty2.ty)) then
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
                if Types.compat init_tyexp.ty ty then lift_ty arrty
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
        | S.For (sym, froml, tol, bodyl) -> (
            (* adding the index to venv, as 'RO' so we can't assign it in the source *)
            let venv' = Symbol.Table.add venv ~key:sym ~data:(Env.VarEntry (Types.Int, false))  in
            let ty = (transExp  venv' tenv bodyl).ty in
            if not @@ Types.compat ty Types.Unit
            then type_error exp.L.loc @@
                sprintf "The body of a For loop must be of Unit type, found %s"
                (Types.to_string ty)
            else (check_int froml; check_int tol; lift_ty Types.Unit)
          )
        (* TODO: check that BREAK is inside a loop *)
        | S.Break _ -> lift_ty Types.Unit
        | S.Assign (vl, el) -> (
            let ret = lift_ty Types.Unit in
            let vartyexp, assign = trLValue vl in
            let varty = vartyexp.ty in
            if assign then (check_ty varty el; ret)
            else type_error exp.L.loc @@
              sprintf "Assigning a RO variable is forbidden (for loop index, function argument)"
        )
        | S.Let (decl, el) ->
            let venv', tenv' = transDecs venv tenv decl in
            transExp venv' tenv' el

    (* trLValue: S.lvalue location -> expty * bool *)
    and trLValue var =
    match var.L.item with
      | S.VarId sl -> begin
          match venv_find sl venv with
          | Env.VarEntry (ty, assign) -> lift_ty ty, assign
          | Env.FunEntry _ ->
            type_error sl.L.loc @@
            sprintf "%s is a function, expected a variable" (Symbol.name sl.L.item)
        end
    | S.FieldAccess (vl, sl) -> (
        let ty = (fst @@ trLValue vl).ty in
        match ty with
            | Types.Record (fields, _) -> (
                try lift_ty (List.Assoc.find_exn fields sl.L.item), true
                with Not_found -> name_error sl.L.loc @@
                                    sprintf "Unknown field %s for record %s"
                                    (Symbol.name sl.L.item) (Types.to_string ty)
            )
            | _ -> type_error vl.L.loc @@
                    sprintf "%s is not of Record type" (Types.to_string ty)
        )
    | S.ArrayAccess (vl, el) -> (
        let ty = (fst @@ trLValue vl).ty in
        match ty with
            | Types.Array (typ, _) -> (
                check_int el;
                lift_ty typ, true
            )
            | _ -> type_error vl.L.loc @@
                    sprintf "%s is not of Array type" (Types.to_string ty)
    )
in trExp exp

(* transDecs: venv -> tenv -> S.dec list -> (venv * tenv) *)
(* Note: tenv must not have Types.Name(None) entries, neither I/O *)
and transDecs venv tenv =
  List.fold_left ~f:(fun (venv', tenv') dec -> transDec venv' tenv' dec)
    ~init:(venv, tenv)

(* transDec: venv -> tenv -> S.dec -> (venv * tenv) *)
(* Note: tenv must not have Types.Name(None) entries, neither I/O *)
and transDec venv tenv = function
  | S.VarDec vl -> (
      let var = vl.L.item in
      let tyexp = transExp venv tenv var.S.value in
      begin
        match var.S.var_type with
        | Some sl -> let styp = tenv_find sl tenv in
          if not @@ Types.compat styp tyexp.ty then
            type_error vl.L.loc @@
            sprintf "%s is incompatible with %s"
              (Types.to_string styp) (Types.to_string tyexp.ty)
        | None -> (* var a := nil is forbidden *) begin
            match var.S.value.L.item with
            | S.Nil _ -> type_error vl.L.loc
                "Can't initialize a variable using 'nil' without an explicit type"
            | _ -> ()
        end
      end;
      Symbol.Table.add venv ~key:var.S.var_name.L.item ~data:(Env.VarEntry (tyexp.ty, true)), tenv
    )
  | S.FunDec funlist ->
    (* gather the headers of each function *)
    let fun_and_header_list =
      List.fold_right
        funlist
        ~f:(fun lfundec acc ->
            let argsty,retty = trans_fun_sig tenv lfundec in
            (lfundec, (argsty, retty)) :: acc)
        ~init:[] in
    (* update the venv with FunEntry for each function *)
    let venv' = List.fold_left
        fun_and_header_list
        ~f:(fun acc (lfundec, (argsty, retty)) ->
            Symbol.Table.add acc
              ~key:lfundec.L.item.S.fun_name.L.item
              ~data:(Env.FunEntry (List.map argsty snd, retty)))
        ~init:venv in
    (* then parse each body with all headers in the environment *)
    List.fold_left fun_and_header_list
      ~f:(fun venv_acc fundec_and_header -> trans_fun venv_acc tenv fundec_and_header)
      ~init:venv', tenv
  | S.TypeDec typlist ->
    let dummy_loc = match typlist with | hd :: _ -> hd.L.loc | [] -> L.dummy_loc in
    if check_typdec_shadowing typlist
    then type_error dummy_loc "Shadowing detected in mutual type definition";
    if check_typdec_cycle typlist
    then type_error dummy_loc "Cycle detected in mutual type definition";
    (* gather the headers of each type, introducing temporary Types.Name(None) *)
    let tenv' =
      List.fold_left
        typlist
        ~f:(fun tenv_acc ltypdec ->
            let typdec = ltypdec.L.item in
            let header = Types.Name (typdec.S.type_name.L.item, ref None) in
            Symbol.Table.add tenv_acc ~key:typdec.S.type_name.L.item ~data:header)
        ~init:tenv in
    (* the parse each type with all headers in the environment *)
    let tenv'' = List.fold_left typlist
      ~f:(fun tenv_acc typdec -> trans_typ tenv_acc typdec)
      ~init:tenv' in
    (* now update the Types.Name(None) -> Types.Name(Some) that this function introduced *)
    venv, fix_name_ty tenv''

and trans_typ tenv ltypdec =
  let typdec = ltypdec.L.item in
  let type_name = typdec.S.type_name in
  let typ = typdec.S.typ in
  Symbol.Table.add tenv ~key:type_name.L.item ~data:(transTy tenv typ)

and trans_fun venv tenv (lfundec, (argsty, retty)) =
  let fundec = lfundec.L.item in
  (* Can't assign variable that are input variable of a function *)
  let venv' = List.fold_left argsty
      ~f:(fun venv_acc (name, ty) -> Symbol.Table.add venv_acc
             ~key:name.L.item ~data:(Env.VarEntry (ty, false)))
      ~init:venv in
  let body_tyexp = transExp venv' tenv fundec.S.body in
  if not @@ phys_equal body_tyexp.ty retty then
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
