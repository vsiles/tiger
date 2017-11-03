type t = int * string

let mk =
    let table = Hashtbl.create 32 in
    let n = ref (-1) in
    function name ->
        try
            Hashtbl.find table name, name
        with Not_found ->
            incr n;
            Hashtbl.add table name !n;
            !n, name
;;

let name (_, s) = s
;;

module Ord = struct
    (* to avoid confusion an write type t = t *)
    type symbol = t
    type t = symbol

    let compare (n1, _) (n2, _) =
        Pervasives.compare n1 n2
end

module Table = Map.Make (Ord)
