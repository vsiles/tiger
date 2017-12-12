open Core.Std

let wordSize = 4

module ARM32Frame : Frame.Frame =
    struct
        let wordSize = 4
        type access = InFrame of int | InReg of Temp.temp
        type frame = {
            formals: access list;
            localCount: int ref;
            name: Temp.label;
            frameSize: int ref;
        }
        let allocLocal frame escape = (* TODO *) InFrame 0

        let newFrame ~name ~formals =
            (* TODO *) { formals = []; localCount = ref 0;
                name = name; frameSize = ref 0; }

        let name frame = frame.name

        let formals frame = frame.formals
    end
