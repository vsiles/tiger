open Errors
open Printf

module L = Location
module TS = Tig_syntax

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

(*
let venv_find = env_find "value"
*)
(*
let fenv_find = env_find "function"
*)
let tenv_find = env_find "type"

type expty = {exp: Translate.exp; ty: Types.t}

(*
transVar: venv * tenv * S.var -> expty
transExp: venv * tenv * S.exp -> expty
transDec: venv * tenv * S.dec -> {venv: venv, tenv: tenv}
transTy: tenv * S.ty -> T.t
*)

let transTy tenv = function
    | TS.TyName sl -> tenv_find sl tenv
    | TS.TyArray sl -> Types.Array (tenv_find sl tenv, Types.new_tag ())
    | TS.TyRecord l -> Types.Record (
        List.map (fun f -> (f.TS.field_name.L.item, tenv_find f.TS.field_type tenv)) l,
        Types.new_tag())
