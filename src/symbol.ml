open Core.Std

type t = int * string

let mk =
  let table = String.Table.create() in
    let n = ref (-1) in
    function name ->
    match Hashtbl.find table name with
    | Some x -> x, name
    | None -> (
        incr n;
        match Hashtbl.add table name !n with
        | `Duplicate -> failwith "Must not happend since .find failed"
        | `Ok -> !n, name
      )
;;

let name (_, s) = s
;;

let equal s1 s2 =
  Pervasives.compare (fst s1) (fst s2) = 0
;;

module Ord = struct
  type symbol = t
  type t = symbol

  let compare (x0, _) (x1, _) = Pervasives.compare x0 x1

  let t_of_sexp tuple = Tuple2.t_of_sexp Int.t_of_sexp String.t_of_sexp tuple
  let sexp_of_t tuple = Tuple2.sexp_of_t Int.sexp_of_t String.sexp_of_t tuple
end

module Table = Map.Make(Ord)
