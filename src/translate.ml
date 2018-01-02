open Core.Std

module T = Tree

(* never to be called on an empty list *)
let rec seq = function
  | hd :: tl -> begin match tl with
      | hd2 :: tl2 -> T.SEQ (hd, seq tl)
      | [] -> hd
        end
  | [] -> failwith "Tree.seq failure"
;;

module type Translate =
    sig
      type exp
      type level
      type access

      val outermost: level
      val newLevel: parent:level -> name:Temp.label ->
        formals:bool list -> level
      val formals: level -> access list
      val allocLocal: level -> bool -> access

      val simpleVar: access -> level -> exp
      val arrayAccess: exp -> exp -> exp
      val fieldAccess: exp -> Symbol.t -> Symbol.t list -> exp
      val intConst: int -> exp
      val binOperation : Syntax.op -> exp -> exp -> exp
      val nilExp: exp
      val unitExp: exp
      val ifthenelse : exp -> exp -> exp -> exp
      val stringExp : string -> exp
      val recordExp: exp list -> exp
      val arrayExp: exp -> exp -> exp
      val whileExp: exp -> exp -> Temp.label -> exp
      val breakExp: Temp.label -> exp
      val forExp: exp -> exp -> exp -> exp -> Temp.label -> exp

      val placeholder: exp

(*      val pp_access: access -> unit *)
    end

module Make (F: Frame.Frame) : Translate = struct

  type exp =
    | Ex of T.exp (* expression *)
    | Nx of T.stm (* void expression *)
    | Cx of (Temp.label -> Temp.label -> T.stm) (* conditionals *)


  let placeholder = Ex (T.CONST 0)

  let unEx = function
    | Ex e -> e
    (*
        we cast to 'int' any 'void' expression as we don't support the
        primitive type 'void' per se. We only support a limited for
        of unitialized structure
    *)
    | Nx s -> T.ESEQ (s, T.CONST 0)
    | Cx fcond ->
      let r = Temp.newtemp ()
      and t = Temp.newlabel ()
      and f = Temp.newlabel ()
      in T.ESEQ (seq [
          T.MOVE (T.TEMP r, T.CONST 1);
          fcond t f;
          T.LABEL f;
          T.MOVE (T.TEMP r, T.CONST 0);
          T.LABEL t], T.TEMP r)
  ;;

  let unNx nx = match nx with
    | Nx s -> s
    | Ex e -> T.EXP e
    | _ -> T.EXP (unEx nx)
  ;;

  let unCx cx = match cx with
    | Ex (T.CONST 0) -> (fun _ l2 -> T.JUMP (T.NAME l2, [l2]))
    | Ex (T.CONST 1) -> (fun l1 _ -> T.JUMP (T.NAME l1, [l1]))
    | Cx fcond -> fcond
    | Ex e -> (fun l1 l2 -> T.CJUMP (T.EQ, T.CONST 1, e, l1, l2))
    | Nx _ -> failwith "Ill-typed program, tried to unCx a Nx expression"
  ;;

  type nested_level = {
      parent: level;
      frame: F.frame;
      id: unit ref;
    }
  and level = Top | Nested of nested_level

  let level_equals level1 level2 = match level1, level2 with
    | Top, Top -> true
    | Nested l1, Nested l2 -> phys_equal l1.id l2.id
    | _, _ -> false
  ;;

  type access = level * F.access
(*
  let pp_access (lvl, _) =
    let s = match lvl with | Top -> "Top" | Nested l -> Symbol.name @@ F.name l.frame in
    printf "<%s>\n" s
  ;;
*)

  let outermost = Top;;

  let newLevel ~parent ~name ~formals = match parent with
    | Nested _ -> begin
        let newframe = F.newFrame ~name:name ~formals:(true :: formals) in
        let ret = Nested { parent = parent; frame = newframe; id = ref () } in
(*        printf "\nnewLevel %s 0x%x\n" (Symbol.name name) (2 * (Obj.magic ret)); *)
        ret
      end
    | Top -> begin
        let newframe = F.newFrame name formals in
        let ret = Nested { parent = parent; frame = newframe; id = ref () } in
(*        printf "\nnewLevel %s 0x%x\n" (Symbol.name name) (2 * (Obj.magic ret)); *)
        ret
      end
  ;;

  let formals lvl = match lvl with
    | Top -> failwith "Must not call 'formals' on outermost level"
    | Nested l -> begin
        let frame = l.frame in
        (* check for static link, if present, remove it *)
        match F.formals frame with
        | [] -> [] (* no static link *)
        | _ :: l -> List.map l (fun acc -> (lvl, acc))
      end
  ;;

  let allocLocal lvl escape = match lvl with
    | Top -> failwith "Must not call 'allocLocal' on outermost level"
    | Nested l -> (lvl, F.allocLocal l.frame escape)
  ;;

  (* follow up static links to compute the correct offset to access a var *)
  let rec follow_links curr target exp =
(*    let p1 = 2 * (Obj.magic curr) in *)
(*    let n1 = match curr with | Top -> "Top" | Nested l -> Symbol.name @@ F.name l.frame in *)
(*    let p2 = 2 * (Obj.magic target) in *)
(*    let n2 = match target with | Top -> "Top" | Nested l -> Symbol.name @@ F.name l.frame in *)
(*    printf "follow_links %s %s (0x%x 0x%x)\n" n1 n2 p1 p2; *)
    match curr with
    | Nested l -> if level_equals curr target then exp
      else begin
        match F.formals l.frame with
        | sl :: _ -> follow_links l.parent target (F.exp sl exp)
        | _ -> failwith "Translate: can't follow missing static link"
      end
    | Top -> failwith "Translate: reached Top while following sl"
  ;;


  (* access -> level -> exp *)
  let simpleVar (target, faccess) curr =
(*    let p1 = 2 * (Obj.magic curr) in *)
(*    let n1 = match curr with | Top -> "Top" | Nested l -> Symbol.name @@ F.name l.frame in *)
(*    let p2 = 2 * (Obj.magic target) in *)
(*    let n2 = match target with | Top -> "Top" | Nested l -> Symbol.name @@ F.name l.frame in *)
(*    printf "simpleVar %s %s (0x%x 0x%x)\n" n1 n2 p1 p2; *)
    Ex (F.exp faccess (follow_links curr target (T.TEMP F.fp)))
  ;;

  let base_n_offset_builder base offset size =
    let n = T.BINOP (T.MUL, offset, size) in
    T.BINOP (T.PLUS, base, n)
  ;;

  (* TODO: add the size of the array to the variable declaration so
           we can do bound checking *)
  (* TODO: add simplification of the size, if possible, to detect
           out of bound at compile time if possible *)
  (* exp -> exp -> exp *)
  let arrayAccess arr ndx =
    let tarr = unEx arr
    and tndx = unEx ndx in
    Ex (T.MEM (base_n_offset_builder tarr tndx (T.CONST (F.wordSize))))
  ;;

  (* find the ndx of a symbol in a list *)
  let rec lfind_pos x pos = function
    | [] -> None
    | hd :: tl ->
      if Symbol.equal x hd = 0
      then Some pos
      else lfind_pos x (pos + 1) tl
  ;;

  (* TODO: add NPE check *)
  (* pre-condition: the provided field must be in the field list *)
  let fieldAccess base field fields =
    let pos = match lfind_pos field 0 fields with
      | None -> failwith "precondition of fieldAccess violated"
      | Some n -> T.CONST n
    and tbase = unEx base in
    Ex (T.MEM (base_n_offset_builder tbase pos (T.CONST (F.wordSize))))
  ;;

  let intConst n = Ex (T.CONST n);;

  type op_info = BinOp of T.binop | RelOp of T.relop

  let op_of_op = function
    | Syntax.Plus -> BinOp T.PLUS
    | Syntax.Minus -> BinOp T.MINUS
    | Syntax.Times -> BinOp T.MUL
    | Syntax.Div -> BinOp T.DIV
    | Syntax.Eq -> RelOp T.EQ
    | Syntax.Neq -> RelOp T.NE
    | Syntax.Lt -> RelOp T.LT
    | Syntax.Le -> RelOp T.LE
    | Syntax.Gt -> RelOp T.GT
    | Syntax.Ge -> RelOp T.GE
  ;;

  (* negate an expression *)
  let negate exp = Cx (fun t f -> T.CJUMP (T.EQ, T.CONST 0, exp, t, f));;

  (* Comparison of strings is done with an external program *)
  let rec stringOperation op lexp rexp = match op with
    | T.EQ -> Ex (F.externalCall "stringEqual" [lexp; rexp])
    | T.NE -> negate (unEx (stringOperation T.EQ lexp rexp))
    | _ -> failwith "stringOperation must only be called on EQ and NE"
  ;;

  let binOperation op lexp rexp =
    let left_exp = unEx lexp
    and right_exp = unEx rexp in
    match op_of_op op with
    | BinOp op -> Ex (T.BINOP (op, left_exp, right_exp))
    | RelOp op -> Cx (fun t f ->
        T.CJUMP (op, left_exp, right_exp, t, f))
  ;;

  let nilExp = Ex (T.CONST 0);;
  let unitExp = Nx (T.EXP (T.CONST 0));;

  let ifthenelse test_exp then_exp else_exp =
    let test = unCx test_exp
    and test' = unEx test_exp in

    let result = Temp.newtemp ()
    and label_then = Temp.newlabel ()
    and label_else = Temp.newlabel ()
    and label_join = Temp.newlabel () in

    match test', then_exp, else_exp with
    (* First check if the test is not trivially true/false *)
    | T.CONST 0, _, _ -> else_exp
    | T.CONST _, _, _ -> then_exp
    (* Special cases to deal with & and | that we translated into if/then/else *)
    |  _, Cx _, Ex (T.CONST 0) -> (* & case *)
      Cx (fun t f ->
          seq [test label_then f;
               T.LABEL label_then;
               (unCx then_exp) t f]
        )
    | _, Ex (T.CONST 1), Cx _ -> (* | case *)
      Cx (fun t f ->
          seq [test t label_else;
               T.LABEL label_else;
               (unCx else_exp) t f]
        )
    (* If the then/else bodies are conditionals too, avoid a label mess *)
    | _, Cx _, _ ->
      Cx (fun t f ->
          seq [test label_then label_else;
               T.LABEL label_then;
               (unCx then_exp) t  f;
               T.LABEL label_else;
               (unCx else_exp) t f]
        )
    (* If the then/else bodies are units, don't bother with computing a result *)
    | _, Nx _, _ ->
      Nx (seq [test label_then label_else;
               T.LABEL label_then;
               unNx then_exp;
               T.JUMP (T.NAME label_join, [label_join]);
               T.LABEL label_else;
               unNx else_exp;
               T.LABEL label_join]
         )
    | _, _, _ ->
      Ex (T.ESEQ (seq [ test label_then  label_else;
                        T.LABEL label_then;
                        T.MOVE (T.TEMP result, unEx then_exp);
                        T.JUMP (T.NAME label_join, [label_join]);
                        T.LABEL label_else;
                        T.MOVE (T.TEMP result, unEx else_exp);
                        T.JUMP (T.NAME label_join, [label_join]);
                        T.LABEL label_join], T.TEMP result))

  ;;

  (* list of fragments *)
  let frags : F.frag list ref = ref [];;

  (* String declaration *)
  let stringExp str =
    let some_label =
      List.find (!frags) (fun f -> match f with
          | F.STRING (lbl, str') -> Pervasives.compare str str' = 0
        ) in
    let label = match some_label with
      | Some (F.STRING (l, _)) -> l
(*      | Some _ -> failwith "Failure in Translate.string" *)
      | None -> let l = Temp.newlabel () in
        frags := F.STRING (l, str) :: (!frags);
        l
    in Ex (T.NAME label)
  ;;

  (* Record allocation *)
  let recordExp inits =
    let initExp r n exp =
      let pos = T.BINOP (T.PLUS, r, T.CONST (n * F.wordSize)) in
      T.MOVE (T.MEM pos, unEx exp)
    and base = T.TEMP (Temp.newtemp ())
    and nr = List.length inits in
    let call_alloc = F.externalCall "allocRecord" [T.CONST (nr * F.wordSize)] in
    let alloc = T.MOVE (base, call_alloc) in
    let (s, _) = List.fold_left inits
        ~init:(alloc, 0) ~f:(fun (acc, i) init ->
            (T.SEQ (acc, initExp base i init)), i + 1)
    in Ex (T.ESEQ (s, base))
  ;;

  (* Array allocation *)
  let arrayExp length init =
    Ex (F.externalCall "initArray" [unEx length; unEx init])

  let whileExp condExp bodyExp done_label =
    let body_label = Temp.newlabel ()
    and cond_label = Temp.newlabel ()
    and test = unCx condExp
    and body = unNx bodyExp in
    Nx (seq [ T.LABEL cond_label;
              test body_label done_label;
              T.LABEL body_label;
              body;
              T.JUMP (T.NAME cond_label, [cond_label]);
              T.LABEL done_label])
  ;;

  let breakExp done_label = Nx (T.JUMP (T.NAME done_label, [done_label]));;

  let forExp varExp lowExp highExp bodyExp done_label =
    let var = unEx varExp
    and low = unEx lowExp
    and high = unEx highExp
    and body = unNx bodyExp
    and body_label = Temp.newlabel ()
    and next_label = Temp.newlabel () in
    Nx (seq [
        T.MOVE (var, low);
        T.CJUMP (T.LE, var, high, body_label, done_label);
        T.LABEL body_label;
        body;
        T.CJUMP (T.LT, var, high, next_label, done_label);
        T.LABEL next_label;
        T.MOVE (var, T.BINOP (T.PLUS, var, T.CONST 1));
        T.JUMP (T.NAME body_label, [body_label]);
        T.LABEL done_label ])
  ;;

end
