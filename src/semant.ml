open Core.Std
open Errors

module L = Location
module S = Syntax
module UF = Core_kernel.Union_find

module type Semant = sig
  val transProg: Syntax.exp -> unit
end

module Make (T: Translate.Translate) : Semant = struct

module E = Env.Make(T)
module Std = Stdlib.Make(T)

type venv = E.entry Symbol.Table.t
type tenv = Types.t Symbol.Table.t

let env_find env_name sym env =
  match Symbol.Table.find env sym.L.item with
  | Some x -> x
  | None ->
    name_error sym.L.loc @@
    sprintf "Unknown %s: %s"
      env_name
      (Symbol.name sym.L.item)

(*
   (* Debug code to print the content of venv *)
let avoid = [ "print";
              "flush";
              "getchar";
              "ord";
              "chr";
              "size";
              "substring";
              "concat";
              "not";
              "exit"];;

let env_pp env =
  Symbol.Table.iteri env
    ~f:(fun ~key ~data -> let n = Symbol.name key in
         if List.mem avoid n then () else begin
           printf "%s: " n;
         match data with
         | E.FunEntry _ -> printf "<fun>\n"
         | E.VarEntry (access, _, _) -> T.pp_access access
         end
       )
;;
*)

(* Symbol.Table.t -> venv -> Env.entry *)
let venv_find = env_find "value"
(* Symbol.Table.t -> venv -> Env.entry *)
let fenv_find = env_find "function"
(* Symbol.Table.t -> tenv -> Types.t *)
let tenv_find = env_find "type"

type expty = {exp: T.exp; ty: Types.t}

(*
    transTy: L.t tenv -> S.ty -> Types.t
    Must not go under Types.Name in order to be able
    to deal with mutual recursive types
*)
let transTy loc tenv typ =
  match typ with
    | S.TyName sl -> tenv_find sl tenv
    | S.TyArray sl -> Types.Array (tenv_find sl tenv, Types.new_tag ())
    | S.TyRecord fields ->
      let names = List.map fields ~f:(fun x -> x.S.field_name.L.item) in
      let _ = if List.contains_dup names ~compare:Symbol.equal then
          syntax_error loc "Duplicate fields in type declaration" in
      let trans_fields = List.map fields
          ~f:(fun f -> (f.S.field_name.L.item, tenv_find f.S.field_type tenv)) in
      Types.Record { Types.fields = trans_fields; Types.tag = Types.new_tag() }
;;

(* Extract name, type & escape information from a fundec::args *)
let rec trans_fundec_args tenv = function
  | hd :: tl ->
    let name = hd.S.field_name and
        typ = tenv_find hd.S.field_type tenv and
        escp = !(hd.S.escape) in
    let l1, l2 = trans_fundec_args tenv tl in
    ((name, typ, escp) :: l1), (escp :: l2)
  | [] -> [], []
;;

(* Helper function to extract type info for function signature *)
let trans_fun_sig tenv lfundec =
  let fundec = lfundec.L.item in
  let argsty, esclist = trans_fundec_args tenv fundec.S.args in
  let retty = match fundec.S.return_type with
    | None -> Types.Unit
    | Some ret -> tenv_find ret tenv in
  argsty, esclist, retty
;;

(* temp function *)
let lift_ty ty = { exp = T.placeholder; ty = ty };;

(* Compare Record fields declaration (annoted by Location)
   to be used to sort them during type checking

   To be used with (Symbol.t Location.loc * exp Location.loc) list
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
          ~f:(fun x -> Symbol.equal type_name x.L.item.S.type_name.L.item = 0)
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
    | Types.Record r -> let fields = r.Types.fields in
      List.iter fields ~f:(fun (_, t) -> fix t)
    | _ -> ()
  in (
    Symbol.Table.iteri tenv (fun ~key ~data -> fix data);
    tenv
  )
;;

let check_eq loc expty =
  match expty.ty with
  | Types.Record _ -> { exp = expty.exp; ty = Types.Int } (* bool is Types.Int *)
  | _ -> type_error loc @@ sprintf
      "Equality test between 'nil' and non record expression (of type %s)"
      (Types.to_string expty.ty)
;;

(* pre-conditions:
   - ref list doesn't have dups
   - inits doesn't have dups
*)
let rec reorder_inits loc inits ref = match ref with
  | (symb, ty) :: tl ->
    let symbl = L.mkloc symb loc in
    let oelt = List.Assoc.find
        inits
        ~equal:(fun sl1 sl2 -> Symbol.equal sl1.L.item sl2.L.item = 0)
        symbl
    in begin match oelt with
    | None ->
      name_error loc @@
      sprintf "Missing field %s initialization" (Symbol.name symb)
    | Some elt -> (elt, ty) :: reorder_inits loc inits tl
    end
  | [] -> []
;;

(* TODO improve error message here,
   might remove some calls to check_* to have more precise log *)
(* transExp: Translate.level -> bool -> venv -> tenv -> S.exp -> expty *)
(* Note: tenv must not have Types.Name(None) entries *)
let rec transExp level allow_break venv tenv exp =
  let rec check_ty ty exp =
    let texp = trExp exp in
    let ty' = texp.ty in
    if not @@ Types.compat ty ty'
    then type_error exp.L.loc @@
      sprintf "%s expected, found %s"
        (Types.to_string ty) (Types.to_string ty')
    else
      texp

    and check_int exp = check_ty Types.Int exp

    and trExp exp = match exp.L.item with
      | S.Lvalue vl -> fst @@ trLValue vl
      | S.Nil _ -> { exp = T.nilExp; ty = Types.Nil }
        (*
            - type of a sequence is the type of the last entry.
            - all entries must typecheck
            - if any entry but the last is of non unit type,
              its value will be discarded
        *)
      | S.Seq ell -> List.fold_left ell
                       ~f:(fun _ el -> trExp el)
                       ~init:(lift_ty Types.Unit)
      | S.Int n -> { exp = T.intConst n.L.item; ty = Types.Int }
      | S.String sl -> {
          exp = T.stringExp sl.L.item;
          ty = Types.String
        }
      | S.FunCall (sl, ell) -> (
          let (argsty, retty) = (match fenv_find sl venv with
              | E.FunEntry (_, _, tylist, ty) -> (tylist, ty)
              | E.VarEntry _ ->
                type_error sl.L.loc @@
                sprintf "%s is a variable, expected a function"
                  (Symbol.name sl.L.item)
            ) in (List.iter2_exn argsty ell
                    (* TODO FIXME *) (fun x y -> let _ = check_ty x y in ()); lift_ty retty)
        )
      | S.BinOp (el1, opl, el2) -> (
          (* TODO improve comparaison support with string comparison *)
          match opl.L.item with
          | S.Eq | S.Neq -> begin
              let expty1 = trExp el1 in
              let expty2 = trExp el2 in
              (* first, check for nil vs nil *)
              match el1.L.item, el2.L.item with
              | S.Nil _, S.Nil _ ->
                type_error exp.L.loc
                  "Spotted equality test between two untyped 'nil', which is forbidden"
              | _, S.Nil _ -> check_eq exp.L.loc expty1
              | S.Nil _, _ -> check_eq exp.L.loc expty2
              | _, _ -> if (not (Types.eq_compat expty1.ty expty2.ty)) ||
                           (not (Types.compat expty1.ty expty2.ty)) then
                  type_error exp.L.loc @@ sprintf
                    "Ill-typed equality test: %s vs %s"
                    (Types.to_string expty1.ty) (Types.to_string expty2.ty)
                else { exp = T.binOperation opl.L.item expty1.exp expty2.exp;
                       ty = Types.Int
                     }
            end
          | _ -> (
              let expty1 = check_int el1
              and expty2 = check_int el2 in
              { exp = T.binOperation opl.L.item expty1.exp expty2.exp;
                ty = Types.Int
              }
            )
        )
      | S.Record (sl, fl) -> (
          let names = List.map fl ~f:(fun f -> (fst f).L.item) in
          let _ = if List.contains_dup names ~compare:Symbol.equal then
              syntax_error sl.L.loc "Duplicate field in Record declaration" in
          let recty = tenv_find sl tenv in begin
            match recty with
            | Types.Record r -> begin
                let fields = r.Types.fields in
                let raw_exps = reorder_inits sl.L.loc fl fields in
                let exps = List.map raw_exps
                    ~f:(fun (expr,ty) ->
                        let expty = check_ty ty expr in expty.exp
                      )
                in { exp = T.recordExp exps; ty = recty }
              end
            | _ -> type_error sl.L.loc @@
              sprintf "%s is not of Record type" (Symbol.name sl.L.item)
          end
        )
      | S.Array (sl, sizel, initl) -> (
          (* TODO check_int *)
          let _ = check_int sizel in
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
      | S.If (testl, thenl, oelsel) -> begin
          let testexp = check_int testl in
          begin match oelsel with
            | None -> (* then then branch my be of type unit *)
              let thenexp = check_ty Types.Unit thenl in
              { exp = T.ifthenelse testexp.exp thenexp.exp T.unitExp;
                ty = Types.Unit
              }
            | Some elsel ->
              let thenexp = trExp thenl in
              let elseexp = check_ty thenexp.ty elsel in
              {
                exp = T.ifthenelse testexp.exp thenexp.exp elseexp.exp;
                ty = thenexp.ty
              }
          end
        end
      | S.While (condl, bodyl) -> (
          (* TODO check_int *)
          let _ = check_int condl in
          let ty = (transExp level true venv tenv bodyl).ty in
          if not @@ Types.compat ty Types.Unit
          then type_error exp.L.loc @@
            sprintf "The body of a While loop must be of Unit type, found %s"
              (Types.to_string ty)
          else lift_ty Types.Unit
        )
      | S.For (sym, escp, froml, tol, bodyl) -> (
          (* adding the index to venv, as 'RO' so we can't assign it in the source *)
          let access = T.allocLocal level !escp in
          (*            let _ = printf "Adding loop index %s" (Symbol.name sym) in *)
          let venv' = Symbol.Table.add venv ~key:sym
              ~data:(E.VarEntry (access, Types.Int, false))  in
          let ty = (transExp level true venv' tenv bodyl).ty in
          if not @@ Types.compat ty Types.Unit
          then type_error exp.L.loc @@
            sprintf "The body of a For loop must be of Unit type, found %s"
              (Types.to_string ty)
              (* TODO check_int *)
          else begin
            let _ = check_int froml in
            let _ = check_int tol in
            lift_ty Types.Unit
          end
        )
      | S.Break _ -> if allow_break then lift_ty Types.Unit
        else type_error exp.L.loc "Found 'break' instruction outside of For/While loop"
      | S.Assign (vl, el) -> (
          let ret = lift_ty Types.Unit in
          let vartyexp, assign = trLValue vl in
          let varty = vartyexp.ty in
          if assign then ((* TODO FIXME *)
            let _ = check_ty varty el in ret)
          else type_error exp.L.loc @@
            sprintf "Assigning a RO variable is forbidden (For loop index, function argument)"
        )
      | S.Let (decl, el) ->
        (*            let _ = printf "Translating Let block\n" in *)
        (*             let _ = env_pp venv in *)
        let venv', tenv' = transDecs level venv tenv decl in
        (*            let _ = printf "Translating Let body\n" in *)
        (*            let _ = env_pp venv' in *)
        let ret = transExp level allow_break venv' tenv' el in
        (*            let _ = printf "Done Translating Let\n" in *)
        ret

    (* trLValue: S.lvalue location -> expty * bool *)
    and trLValue var = match var.L.item with
      | S.VarId sl -> begin
          match venv_find sl venv with
          | E.VarEntry (access, ty, assign) ->
            (* printf "\nProcessing %s\n" (Symbol.name sl.L.item); *)
            { exp = T.simpleVar access level; ty = ty}, assign
          | E.FunEntry _ ->
            type_error sl.L.loc @@
            sprintf "%s is a function, expected a variable" (Symbol.name sl.L.item)
        end
      | S.FieldAccess (vl, sl) -> (
          let field = sl.L.item in
          (* TODO: add compile type check to rule out NPE when possible *)
          let struct_exp = fst @@ trLValue vl in
          let ty = struct_exp.ty in
          match ty with
          | Types.Record r -> (
              try
                let fields = r.Types.fields in
                let names = List.map fields ~f:fst in
                let final_ty = List.Assoc.find_exn fields field in
                { exp = T.fieldAccess struct_exp.exp field names;
                  ty = final_ty }, true
              with Not_found -> name_error sl.L.loc @@
                sprintf "Unknown field %s for record %s"
                  (Symbol.name field) (Types.to_string ty)
            )
          | _ -> type_error vl.L.loc @@
            sprintf "%s is not of Record type" (Types.to_string ty)
        )
      | S.ArrayAccess (vl, el) -> (
          (* TODO: add compile type check to rule out NPE when possible *)
          let arr_exp = fst @@ trLValue vl in
          let ty = arr_exp.ty in
          match ty with
          | Types.Array (typ, _) -> (
              let ndx = check_int el in
              let texp = T.arrayAccess arr_exp.exp ndx.exp in
              { exp = texp; ty = typ }, true
            )
          | _ -> type_error vl.L.loc @@
            sprintf "%s is not of Array type" (Types.to_string ty)
        )
in trExp exp

(* transDecs: Translate.level -> venv -> tenv -> S.dec list -> (venv * tenv) *)
(* Note: tenv must not have Types.Name(None) entries, neither I/O *)
and transDecs level venv tenv =
  List.fold_left ~f:(fun (venv', tenv') dec -> transDec level venv' tenv' dec)
    ~init:(venv, tenv)

(* transDec: Translate.level -> venv -> tenv -> S.dec -> (venv * tenv) *)
(* Note: tenv must not have Types.Name(None) entries, neither I/O *)
and transDec level venv tenv = function
  | S.VarDec vl -> (
      let var = vl.L.item in
      let escp = var.S.escape in
      let access = T.allocLocal level !escp in
      let tyexp = transExp level false venv tenv var.S.value in
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
(*      let _ = printf "Adding new let variable %s\n" (Symbol.name var.S.var_name.L.item) in *)
      Symbol.Table.add venv
        ~key:var.S.var_name.L.item
        ~data:(E.VarEntry (access, tyexp.ty, true)), tenv
    )
  | S.FunDec funlist ->
    (* gather the headers of each function. Also create label & level *)
    let fun_and_header_list =
      List.fold_right
        funlist
        ~f:(fun lfundec acc ->
            let argsty,esclist,retty = trans_fun_sig tenv lfundec in
(*            let _ = printf "\nNew function %s" (Symbol.name lfundec.L.item.S.fun_name.L.item) in *)
            let name = Temp.newlabel() in
            let newlvl = T.newLevel ~parent:level ~name ~formals:esclist in
            (lfundec, (argsty, retty), name, newlvl) :: acc)
        ~init:[] in
    (* update the venv with FunEntry for each function *)
    let venv' = List.fold_left
        fun_and_header_list
        ~f:(fun acc (lfundec, (argsty, retty), name, newlvl) ->
            let tylist = List.map argsty (fun (_, x, _) -> x) in
            Symbol.Table.add acc
              ~key:lfundec.L.item.S.fun_name.L.item
              ~data:(E.FunEntry (newlvl, name, tylist, retty)))
        ~init:venv in
    (* then parse each body with all headers in the environment *)
    List.iter fun_and_header_list
      ~f:(fun fundec_and_header -> trans_fun venv' tenv fundec_and_header);
      venv', tenv
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
  let tloc = ltypdec.L.loc
  and typdec = ltypdec.L.item in
  let type_name = typdec.S.type_name in
  let typ = typdec.S.typ in
  Symbol.Table.add tenv ~key:type_name.L.item ~data:(transTy tloc tenv typ)

and trans_fun venv tenv (lfundec, (argsty, retty), name, level) =
(*  let _ = printf "Translating %s\n" (Symbol.name lfundec.L.item.S.fun_name.L.item) in *)
  let fundec = lfundec.L.item in
  let venv' = List.fold_left argsty
      ~f:(fun venv_acc (name, ty, escp) ->
          let access = T.allocLocal level escp in
(*          let _ = printf "Adding new argument %s\n" (Symbol.name name.L.item) in *)
            Symbol.Table.add venv_acc
              (* Can't assign variable that are input variable of a function *)
              ~key:name.L.item ~data:(E.VarEntry (access, ty, false)))
      ~init:venv in
  let body_tyexp = transExp level false venv' tenv fundec.S.body in
(*  let _ = printf "Done\n" in  *)
  if not @@ phys_equal body_tyexp.ty retty then
      type_error lfundec.L.loc @@
      sprintf "The body of this function is of type %s, not %s"
        (Types.to_string body_tyexp.ty) (Types.to_string retty)
;;

(* transProg: Syntax.exp -> unit *)
let transProg exp =
  let lexp = L.mkdummy exp in
  let venv = Std.init E.base_venv in
  let mainlvl = T.newLevel
      ~parent:T.outermost
      ~name:(Temp.namedlabel "__main")
      ~formals:[] in
  let _ = transExp mainlvl false venv E.base_tenv lexp in ()

end
