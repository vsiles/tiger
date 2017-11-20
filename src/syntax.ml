open Errors
open Location
open Core.Std

type op =
    | Plus
    | Minus
    | Times
    | Div
    | Eq
    | Neq
    | Lt
    | Le
    | Gt
    | Ge

let translate_op l = function
    | Tig_syntax.Plus -> Plus
    | Tig_syntax.Minus -> Minus
    | Tig_syntax.Times -> Times
    | Tig_syntax.Div -> Div
    | Tig_syntax.Eq -> Eq
    | Tig_syntax.Neq -> Neq
    | Tig_syntax.Lt -> Lt
    | Tig_syntax.Le -> Le
    | Tig_syntax.Gt -> Gt
    | Tig_syntax.Ge -> Ge
    | Tig_syntax.And -> syntax_error l "And must be translated to If/Then/Else"
    | Tig_syntax.Or -> syntax_error l "Or must be translated to If/Then/Else"
;;

type tyfield = {
    field_name: Symbol.t Location.loc;
    field_type: Symbol.t Location.loc;
}

let translate_field f = {
  field_name = f.Tig_syntax.field_name;
  field_type = f.Tig_syntax.field_type;
  };;

(* For TyRecord, tyfield list must be sorted by field name *)
type ty =
    | TyName of Symbol.t Location.loc
    | TyRecord of tyfield list
    | TyArray of Symbol.t Location.loc

let field_cmp field1 field2 =
  let name1 = field1.Tig_syntax.field_name in
  let name2 = field2.Tig_syntax.field_name in
  Pervasives.compare
    (Symbol.name name1.Location.item)
    (Symbol.name name2.Location.item)
;;

let translate_ty ty = match ty with
  | Tig_syntax.TyName sl -> TyName sl
  | Tig_syntax.TyRecord fields -> let sorted_list = (List.sort ~cmp:field_cmp fields)
    in TyRecord (List.map ~f:translate_field sorted_list)
  | Tig_syntax.TyArray sl -> TyArray sl
;;

type exp =
    | Lvalue of lvalue Location.loc
    | Nil of unit Location.loc
    | Seq of exp Location.loc list
    | Int of int Location.loc
    | String of string Location.loc
    | FunCall of Symbol.t Location.loc * exp Location.loc list
    | BinOp of exp Location.loc * op Location.loc * exp Location.loc
    | Record of Symbol.t Location.loc * (Symbol.t Location.loc * exp Location.loc) list
    | Array of Symbol.t Location.loc * exp Location.loc * exp Location.loc
    | If of exp Location.loc * exp Location.loc * exp Location.loc option
    | While of exp Location.loc * exp Location.loc
    | For of Symbol.t * exp Location.loc * exp Location.loc *
        exp Location.loc
    | Break of unit Location.loc
    | Assign of lvalue Location.loc * exp Location.loc
    | Let of dec list * exp Location.loc


and lvalue =
    | VarId of Symbol.t Location.loc
    | FieldAccess of lvalue Location.loc * Symbol.t Location.loc
    | ArrayAccess of lvalue Location.loc * exp Location.loc

and fundec = {
    fun_name: Symbol.t Location.loc;
    args: tyfield list;
    return_type : Symbol.t Location.loc option;
    body: exp Location.loc;
}

and vardec = {
    var_name: Symbol.t Location.loc;
    var_type: Symbol.t Location.loc option;
    value: exp Location.loc;
}

and typedec = {
  type_name : Symbol.t Location.loc;
  typ : ty;
}

and dec =
  | FunDec of fundec Location.loc list
  | VarDec of vardec Location.loc
  | TypeDec of typedec Location.loc list

let translate_tydec tdec = {
    type_name = tdec.Tig_syntax.type_name;
    typ = translate_ty tdec.Tig_syntax.typ;
  };;

let rec translate = function
    | Tig_syntax.Lvalue vl -> Lvalue (mkloc (translate_lvalue vl.item) vl.loc)
    | Tig_syntax.Nil ul -> Nil ul
    | Tig_syntax.Seq ell -> Seq (List.map ell ~f:(fun el -> mkloc (translate el.item) el.loc))
    | Tig_syntax.Int il -> Int il
    | Tig_syntax.String sl -> String sl
    | Tig_syntax.FunCall (sl, ell) ->
            FunCall (sl, List.map ell ~f:(fun el -> mkloc (translate el.item) el.loc))

    | Tig_syntax.BinOp (el1, ol, el2) -> (
        let nel1 = mkloc (translate el1.item) el1.loc in
        let nel2 = mkloc (translate el2.item) el2.loc in
        let zero = mkdummy (Int (mkdummy 0)) in
        let one = mkdummy (Int (mkdummy 1)) in
            match ol.item with
                | Tig_syntax.And -> If (nel1, nel2, Some zero)
                | Tig_syntax.Or -> If (nel1, one, Some nel2)
                | _ -> BinOp (nel1, mkloc (translate_op ol.loc ol.item) ol.loc, nel2)
    )
    | Tig_syntax.Record (sl, slell) ->
            Record (sl, List.map slell ~f:(fun (sl, el) -> (sl, mkloc (translate el.item) el.loc)))
    | Tig_syntax.Array (sl, el1, el2) ->
            Array (sl, mkloc (translate el1.item) el1.loc, mkloc (translate el2.item) el2.loc)
    | Tig_syntax.If (el1, el2, elop) ->
            If (mkloc (translate el1.item) el1.loc, mkloc (translate el2.item) el2.loc,
            match elop with None -> None | Some el -> Some (mkloc (translate el.item) el.loc))
    | Tig_syntax.While (el1, el2) -> While (mkloc (translate el1.item) el1.loc,
        mkloc (translate el2.item) el2.loc)
    | Tig_syntax.For (s, el1, el2, el3) -> For (s,
        mkloc (translate el1.item) el1.loc,
        mkloc (translate el2.item) el2.loc,
        mkloc (translate el3.item) el3.loc)
    | Tig_syntax.Break ul -> Break ul
    | Tig_syntax.Assign (vl, el) -> Assign (mkloc (translate_lvalue vl.item) vl.loc,
        mkloc (translate el.item) el.loc)
    | Tig_syntax.Let (dl, el) -> Let (List.map dl ~f:(fun d -> translate_dec d),
        mkloc (translate el.item) el.loc)
    | Tig_syntax.UnaryMinus el -> BinOp ((mkdummy (Int (mkdummy 0))), mkdummy Minus,
        mkloc (translate el.item) el.loc)

and translate_lvalue = function
    | Tig_syntax.VarId sl -> VarId sl
    | Tig_syntax.FieldAccess (vl, sl) -> FieldAccess (mkloc (translate_lvalue vl.item) vl.loc, sl)
    | Tig_syntax.ArrayAccess (vl, el) -> ArrayAccess (mkloc (translate_lvalue vl.item) vl.loc,
        mkloc (translate el.item) el.loc)

and translate_fundec fdec = {
    fun_name = fdec.Tig_syntax.fun_name;
    args = List.map ~f:translate_field fdec.Tig_syntax.args;
    return_type = fdec.Tig_syntax.return_type;
    body = mkloc (translate fdec.Tig_syntax.body.item) fdec.Tig_syntax.body.loc
}

and translate_vardec vdec = {
    var_name = vdec.Tig_syntax.var_name;
    var_type = vdec.Tig_syntax.var_type;
    value = mkloc (translate vdec.Tig_syntax.value.item) vdec.Tig_syntax.value.loc
}

and translate_dec = function
    | Tig_syntax.FunDec fll -> FunDec (List.map fll ~f:(fun fl -> mkloc (translate_fundec fl.item) fl.loc))
    | Tig_syntax.VarDec vl -> VarDec (mkloc (translate_vardec vl.item) vl.loc)
    | Tig_syntax.TypeDec tll -> TypeDec (List.map tll (fun tl -> mkloc (translate_tydec tl.item) tl.loc))
;;
