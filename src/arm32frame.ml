open Core.Std

(*
    To be able to make a more precise frame implementation
    I would need the type of the variable to allocate. So in the
    end:
    - intergers & bool are passed by copy into the frame
    - string, struct & array are passed by reference: they address
      is copied into the frame 

    See ARM calling convention for more details. But since everything will
    be 1 word length, I can simply fill r0-r3 and then add on the stack
*)
module ARM32Frame : Frame.Frame =
    struct
      let wordSize = 4
      let numArgRegs = 4
      type access = InFrame of int | InReg of Temp.temp
      type frame = {
        formals: access list;
        localCount: int ref;
        name: Temp.label;
        frameSize: int ref;
      }

      let allocLocal frame escape =
        let lc = frame.localCount in
        let size = frame.frameSize in
        incr lc;
        if escape then begin
          size := !size - wordSize;
          InFrame (!size);
        end else
          InReg (Temp.newtemp())
      ;;


      (* bool -> (access list * int * int) -> (access list * int * int)  *)
      let allocFormal escp (access_list, nth, offset) =
        if nth < numArgRegs then begin (* still some room in the registers *)
          if escp then (* but we go to stack anyway, in callee *)
            let offset' = offset - wordSize in
            ((InFrame offset) :: access_list), nth + 1, offset'
          else
            (* yeay, register time ! *)
            ((InReg (Temp.newtemp())) :: access_list), nth + 1, offset
        end else (* no room left in registers, args are stored in the caller stack *)
          ((InFrame ((nth - numArgRegs + 1) * wordSize)) :: access_list),
          nth + 1, offset
      ;;

      let newFrame ~name ~formals =
        let (accesses, _, offset) = List.fold_left formals
            ~f:(fun acc b -> allocFormal b acc) ~init:([], 0, 0) in
        {
          formals = List.rev accesses; (* fold_left reverses the initial list *)
          localCount = ref 0;
          name = name;
          frameSize = ref offset;
        }
      ;;

      let name frame = frame.name;;

      let formals frame = frame.formals;;
    end
