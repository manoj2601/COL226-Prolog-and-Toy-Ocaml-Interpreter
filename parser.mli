type token =
  | FLOAT of (float)
  | INT of (int)
  | IF
  | VARIABLE of (string)
  | STRING of (string)
  | SEMICOLON
  | PARENTHESISOPEN
  | PARENTHESISCLOSE
  | COMMA
  | COLON
  | INVALID
  | ENDOFFILE
  | EOL

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Backend.clause
