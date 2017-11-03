type t = {
  startpos : Lexing.position;
  endpos : Lexing.position;
}

let dummy = {
  startpos = Lexing.dummy_pos;
  endpos = Lexing.dummy_pos;
}

type 'a loc = {
  item : 'a;
  loc : t;
}

let mk startpos endpos =
  { startpos; endpos }

let mkloc item loc =
  { item; loc }

let dummy_loc =
    {   startpos = Lexing.dummy_pos;
        endpos = Lexing.dummy_pos
    }

let mkdummy item =
    { item; loc = dummy_loc }
