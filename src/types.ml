open Core.Std
open Errors

(*
 * to be used to distinguish two record/array with the same definitions.
 * The only mean for two types to be equal is to be defined as alias:
 * type t1 = t2
 *)
type tag = int

let new_tag =
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
            | None -> type_error (Location.dummy) ("Mutual type failure (unroll): "^name)
    )
    | _ as foo -> foo
;;

(* Check type compatibility:
   - Nil is compatible with any Record type
   - take care of type unrolling
   - Do no use physical equality (might be issues with cyclic definition
     but rely on the tag *)
let rec compat t1 t2 =
  let ut1 = unroll t1 in
  let ut2 = unroll t2 in
  match ut1, ut2 with
  | Record _, Nil
  | Nil, Record _ -> true
  | Record (_, tag1), Record (_, tag2)
  | Array (_, tag1), Array (_, tag2) -> Pervasives.compare tag1 tag2 = 0
  | Int, Int
  | String, String
  | Nil, Nil
  | Unit, Unit -> true
  | Name _, _
  | _, Name _ -> failwith "Can't happen because unroll should failed first"
  | _, _ -> false
;;

(* Check for boolean operations:
   - Arithmetic: + - * / require integer arguments
   - Comparison < > >= <= require integer arguments
   - Comparison = <> require integer, string, record or array arguments
*)
let eq_compat t1 t2 =
  let ut1 = unroll t1 in
  let ut2 = unroll t2 in
  match ut1, ut2 with
  | Int, Int -> true
  | String, String -> true
  | Record _, Record _ -> true
  | Array _, Array _ -> true
  | _ , _ -> false
;;

let rec to_string = function
    | Int -> "int"
    | String -> "string"
    | Record (l, n) -> sprintf "Record %d: { %s }" n (to_string_list l)
    | Array (t, n) -> sprintf "Array %s %d" (to_string t) n
    | Nil -> "nil"
    | Unit -> "unit"
    | Name (sym, optty_ref) -> begin
        match !optty_ref with
        | Some _ -> sprintf "%s (Some)" (Symbol.name sym)
        | None -> sprintf "%s (None)" (Symbol.name sym)
      end

and to_string_list = function
    | [] -> ""
    | (s, t) :: [] -> (Symbol.name s)^" : "^(to_string t)
    | (s, t) :: tl -> List.fold_left tl
        ~f:(fun str (s, t) -> str^"; "^(Symbol.name s)^" : "^(to_string t))
        ~init:((Symbol.name s)^" : "^(to_string t))
;;
