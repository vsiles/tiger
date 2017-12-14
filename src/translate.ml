open Core.Std

module type Translate =
    sig
        type exp = unit (* pour l'instant *)

        type level
        type access

        val outermost: level
        val newLevel: parent:level -> name:Temp.label ->
            formals:bool list -> level
        val formals: level -> access list
        val allocLocal: level -> bool -> access
    end

module Make (F: Frame.Frame) : Translate = struct

    type exp = unit (* pour l'instant *)

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
