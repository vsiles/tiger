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
    end

module Make (F: Frame.Frame) : Translate = struct

  type exp =
    | Ex of T.exp (* expression *)
    | Nx of T.stm (* void expression *)
    | Cx of (Temp.label -> Temp.label -> T.stm) (* conditionals *)


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

  let outermost = Top;;

  let newLevel ~parent ~name ~formals = match parent with
    | Nested _ -> begin
        let newframe = F.newFrame ~name:name ~formals:(true :: formals) in
        Nested { parent = parent; frame = newframe; id = ref () }
      end
    | Top -> begin
        let newframe = F.newFrame name formals in
        Nested { parent = parent; frame = newframe; id = ref () }
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
    Ex (F.exp faccess (follow_links target curr (T.TEMP F.fp)))
  ;;
end
