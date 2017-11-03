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
    | And
    | Or

type tyfield = {
    field_name: Symbol.t Location.loc;
    field_type: Symbol.t Location.loc;
}

type ty =
    | TyName of Symbol.t Location.loc
    | TyRecord of tyfield list
    | TyArray of Symbol.t Location.loc

type exp =
    | Lvalue of lvalue Location.loc
    | Nil of unit Location.loc
    | Seq of exp Location.loc list
    | Int of int Location.loc
    | String of string Location.loc
    | FunCall of Symbol.t Location.loc * exp Location.loc list
    | UnaryMinus of exp Location.loc
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
