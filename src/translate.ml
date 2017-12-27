open Core.Std

module T = Tree

module type Translate =
    sig
      type exp =
        | Ex of T.exp (* expression *)
        | Nx of T.stm (* void expression *)
        | Cx of (Temp.label -> Temp.label -> T.stm) (* conditionals *)

      type level
      type access

      val outermost: level
      val newLevel: parent:level -> name:Temp.label ->
        formals:bool list -> level
      val formals: level -> access list
      val allocLocal: level -> bool -> access
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
      in T.ESEQ (T.seq [
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

  type level = Top | Nested of (level (* parent *) * F.frame)
  type access = level * F.access

  let outermost = Top;;

  let newLevel ~parent ~name ~formals = match parent with
    | Nested _ -> begin
        let newframe = F.newFrame ~name:name ~formals:(true :: formals) in
        Nested (parent, newframe)
      end
    | Top -> begin
        let newframe = F.newFrame name formals in
        Nested (parent, newframe)
      end
  ;;

  let formals lvl = match lvl with
    | Top -> failwith "Must not call 'formals' on outermost level"
    | Nested (_, frame) -> begin
        (* check for static link, if present, remove it *)
        match F.formals frame with
        | [] -> [] (* no static link *)
        | _ :: l -> List.map l (fun acc -> (lvl, acc))
      end
  ;;

  let allocLocal lvl escape = match lvl with
    | Top -> failwith "Must not call 'allocLocal' on outermost level"
    | Nested (parent, frame) -> (lvl, F.allocLocal frame escape)
  ;;
end
