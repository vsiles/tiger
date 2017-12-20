%{
  open Lexing
  module S = Tig_syntax 
  module L = Location
%}

%token While For To Break Let In End Function Var
%token Type Array If Then Else Do Of Nil
%token Comma Colon Semi Dot
%token LPar RPar LBracket RBracket LBrace RBrace
%token Plus Minus Times Div
%token Eq Neq Lt Le Gt Ge
%token Ampersand Pipe ColonEq
%token Eof

%token <int> Int
%token <string> String
%token <string> Id

%nonassoc loop
%right Else
%nonassoc ColonEq
%right LBracket
%left Pipe
%left Ampersand
%nonassoc Eq Neq Gt Ge Lt Le
%left Plus Minus
%left Times Div
%left UnaryMinus

%start <Tig_syntax.exp> prog

%%

%inline loc(X) :
    | x = X { L.mkloc x (L.mk $startpos $endpos) }

symbol :
    | x = loc(Id) { L.mkloc (Symbol.mk x.L.item) x.L.loc }

prog :
    | e = exp Eof { e }
    | error { Errors.syntax_error (L.mk $startpos $endpos) "" }

exp :
    | var = loc(lvalue) { S.Lvalue var }
    | unit = loc(Nil) { S.Nil unit }
    | LPar seq = separated_list(Semi, loc(exp)) RPar { S.Seq seq }
    | n = loc(Int) { S.Int n }
    | _minus = Minus e = loc(exp) {
        S.UnaryMinus e
      } %prec UnaryMinus
    | e1 = loc(exp) o = loc(op) e2 = loc(exp) { S.BinOp (e1, o, e2) }
    | s = loc(String) { S.String s }
    | fn = symbol LPar formals = separated_list(Comma, loc(exp)) RPar {
        S.FunCall (fn, formals)
    }
    | typ = symbol LBrace fields = separated_list(Comma, field_assign) RBrace {
        S.Record (typ, fields)
    }
    | typ = symbol LBracket size = loc(exp) RBracket Of init = loc(exp) {
        S.Array (typ, size, init)
    } %prec LBracket
    | var = loc(lvalue) ColonEq exp = loc(exp) {
        S.Assign (var, exp)
    }
    | If cond = loc(exp) Then if_true = loc(exp) {
        S.If (cond, if_true, None)
    } %prec Else
    | If cond = loc(exp) Then if_true = loc(exp) Else if_false = loc(exp) {
        S.If (cond, if_true, Some if_false)
    }
    | While cond = loc(exp) Do body = loc(exp) {
        S.While (cond, body)
    } %prec loop
    | For i = symbol ColonEq from = loc(exp) To to_ = loc(exp) Do body = loc(exp) {
        S.For (i.L.item, ref false, from, to_, body)
    } %prec loop
    | unit = loc(Break) { S.Break unit }
    | Let decs = decs In body = loc(separated_list(Semi, loc(exp))) End {
        S.Let (decs, L.mkloc (S.Seq body.L.item) body.L.loc)
    }

lvalue :
    | x = symbol { S.VarId x } %prec LBracket
    | var = loc(lvalue) Dot field = symbol {
        S.FieldAccess (var, field)
    }
    | var = symbol LBracket subscript = loc(exp) RBracket {
        (* Redundant but needed to solve a conflict *)
        S.ArrayAccess ( L.mkloc (S.VarId var) var.L.loc, subscript)
    }
    | var = loc(lvalue) LBracket subscript = loc(exp) RBracket {
        S.ArrayAccess (var, subscript)
    }

%inline op :
    | Plus { S.Plus }
    | Minus { S.Minus }
    | Times { S.Times }
    | Div { S.Div }
    | Eq { S.Eq }
    | Neq { S.Neq }
    | Gt { S.Gt }
    | Ge { S.Ge }
    | Lt { S.Lt }
    | Le { S.Le }
    | Ampersand { S.And }
    | Pipe { S.Or }

field_assign :
    | name = symbol Eq exp = loc(exp) { (name, exp) }

(* Thanks to:
 * - https://github.com/nojb/llvm-tiger/
 * - https://stackoverflow.com/questions/47410781/how-to-parse-list-of-expression-using-menhir/
 * for understanding how to parse the mutual function/type definitions
 *)
decs:
  | hd = loc(vardec) tl = decs_vtf { (S.VarDec hd) :: tl }
  | hd = nonempty_list(loc(tydec)) tl = decs_vf { (S.TypeDec hd) :: tl }
  | hd = nonempty_list(loc(fundec)) tl = decs_vt { (S.FunDec hd) :: tl }

decs_vtf:
  | { [] }
  | hd = loc(vardec) tl = decs_vtf { (S.VarDec hd) :: tl }
  | hd = nonempty_list(loc(tydec)) tl = decs_vf { (S.TypeDec hd) :: tl }
  | hd = nonempty_list(loc(fundec)) tl = decs_vt { (S.FunDec hd) :: tl }

decs_vf:
  | { [] }
  | hd = loc(vardec) tl = decs_vtf { (S.VarDec hd) :: tl }
  | hd = nonempty_list(loc(fundec)) tl = decs_vt { (S.FunDec hd) :: tl }

decs_vt:
  | { [] }
  | hd = loc(vardec) tl = decs_vtf { (S.VarDec hd) :: tl }
  | hd = nonempty_list(loc(tydec)) tl = decs_vf { (S.TypeDec hd) :: tl }

%inline tydec :
    | Type type_name = symbol Eq typ = ty { S.{ type_name; typ } }

ty :
    | t = symbol { S.TyName t }
    | LBrace fields = tyfields RBrace { S.TyRecord fields }
    | Array Of ty = symbol { S.TyArray ty }

tyfields :
    | fields = separated_list(Comma, tyfield) { fields }

tyfield :
    | name = symbol Colon typ = symbol {
        S.{ field_name = name; escape = ref false; field_type = typ }
    }

vardec :
    | Var var_name = symbol ColonEq init = loc(exp) {
        S.{ var_name; escape = ref false; var_type = None; value = init }
    }
    | Var var_name = symbol Colon var_type = symbol ColonEq init = loc(exp) {
        S.{ var_name; escape = ref false; var_type = Some var_type; value = init; }
    }

%inline fundec :
    | Function fun_name = symbol LPar params = tyfields RPar
        Eq body = loc(exp) {
        S.{ fun_name; args = params; return_type = None; body }
    }
    | Function fun_name = symbol LPar params = tyfields RPar
        Colon result_type = symbol Eq body = loc(exp) {
        S.{ fun_name; args = params; return_type = Some result_type; body }
    }
