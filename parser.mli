type token =
  | CHANGESPEC of (int*int)
  | FILEDEF of (string)
  | CHANGEDEF
  | EOF

val file :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Types.filespeclist
val filespeclist :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Types.filespeclist
