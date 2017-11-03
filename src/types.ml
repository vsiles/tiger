open Printf
open Errors

(*
 * to be used to distinguish two record/array with the same definitions.
 * The only mean for two types to be equal is to be defined as alias:
 * type t1 = t2
 *)
type tag = int

let create =
  let n = ref (-1) in
  function () ->
    incr n;
    !n

(*
 * - Nil is here to type the nil value (record)
 * - Unit is here to type statement without values
 * - Name helps to deal with 'not yet declared' mutual types:
 *   + Name(sym, ref(Some(t))) == t
 *   + Name(sym, ref(None)) is a place-holder
 *)
type t =
    | Int
    | String
    | Record of (Symbol.t * t) list * tag
    | Array of t * tag
    | Nil
    | Unit
    | Name of Symbol.t * t option ref

let rec unroll = function
    | Name (symbol, opt) -> (
        let name = Symbol.name symbol in
        match !opt with
            | Some typ -> unroll typ
            | None -> raise (TypeError  ("Mutual type failure (unroll): "^name))
    )
    | _ as foo -> foo

let rec to_string = function
    | Int -> "int"
    | String -> "string"
    | Record (l, n) -> sprintf "Record %d: { %s }" n (to_string_list l)
    | Array (t, n) -> sprintf "Array %s %d" (to_string t) n
    | Nil -> "nil"
    | Unit -> "unit"
    | Name (sym, _) -> Symbol.name sym

and to_string_list = function
    | [] -> ""
    | (s, t) :: [] -> (Symbol.name s)^" : "^(to_string t)
    | (s, t) :: tl -> List.fold_left
        (fun str (s, t) -> str^"; "^(Symbol.name s)^" : "^(to_string t))
        ((Symbol.name s)^" : "^(to_string t))
        tl

