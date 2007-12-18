{
        open Parser        (* The type token is defined in parser.mli *)
        open Printf
        exception Eof


        (* Wonder if there's a better way of doing this. *)
        type lexstates = Global | Filespec | Changespec
        let lexstate = ref Global
}

rule scanner = parse
  | "" 
  { 
          match !lexstate with
          | Global -> globscan lexbuf
          | Filespec -> filespec lexbuf
          | Changespec -> changespec lexbuf
  }
and
globscan = parse
          | "+++" { lexstate:= Filespec;filespec lexbuf}
          (* For some *odd* reason "@@" doesn't work *)
          | '@''@' { lexstate:= Changespec;changespec lexbuf}
          | eof {EOF}
          | '\n' {globscan lexbuf}
          | _ {let _=find_eol lexbuf in scanner lexbuf}
and
filespec = parse
  | [' ''\t'] {filespec lexbuf}
  | ([^' ''\n''\t']+ as fname) {lexstate:=Global;FILEDEF(fname)}
  | _ {lexstate:=Global;globscan lexbuf}
and
changespec = parse
  | [' ''\t'] {changespec lexbuf}
  | ['+''-'](['0'-'9']+ as location)','(['0'-'9']+ as change)  {
          CHANGESPEC(int_of_string location, int_of_string change) }
          | "@@" {lexstate:=Global;globscan lexbuf}
and
find_eol = parse
  | ['\n''\r'] { () }
  | _ {find_eol lexbuf}
