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

open Parsing;;
let _ = parse_error;;
# 2 "parser.mly"
open Backend
exception InvalidInput
exception Error
# 23 "parser.ml"
let yytransl_const = [|
  259 (* IF *);
  262 (* SEMICOLON *);
  263 (* PARENTHESISOPEN *);
  264 (* PARENTHESISCLOSE *);
  265 (* COMMA *);
  266 (* COLON *);
  267 (* INVALID *);
  268 (* ENDOFFILE *);
  269 (* EOL *);
    0|]

let yytransl_block = [|
  257 (* FLOAT *);
  258 (* INT *);
  260 (* VARIABLE *);
  261 (* STRING *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\002\000\002\000\003\000\003\000\006\000\006\000\
\005\000\005\000\004\000\004\000\000\000"

let yylen = "\002\000\
\002\000\001\000\001\000\003\000\001\000\004\000\001\000\001\000\
\001\000\003\000\001\000\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\002\000\013\000\000\000\000\000\000\000\
\001\000\000\000\007\000\008\000\000\000\000\000\000\000\004\000\
\006\000\000\000\000\000\010\000\012\000"

let yydgoto = "\002\000\
\005\000\006\000\012\000\016\000\013\000\014\000"

let yysindex = "\001\000\
\000\255\000\000\001\255\000\000\000\000\253\254\010\255\255\254\
\000\000\009\255\000\000\000\000\007\255\008\255\011\255\000\000\
\000\000\255\254\009\255\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\254\254\000\000\000\000\000\000\003\255\000\000\
\000\000\000\000\000\000\000\000\000\000\013\255\006\255\000\000\
\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\255\255\003\000\005\000\000\000"

let yytablesize = 23
let yytable = "\007\000\
\005\000\001\000\011\000\003\000\003\000\005\000\005\000\008\000\
\015\000\009\000\005\000\004\000\010\000\003\000\017\000\003\000\
\018\000\015\000\011\000\019\000\009\000\021\000\020\000"

let yycheck = "\001\000\
\003\001\001\000\004\001\005\001\005\001\008\001\009\001\007\001\
\010\000\013\001\013\001\012\001\003\001\005\001\008\001\013\001\
\009\001\019\000\013\001\009\001\008\001\019\000\018\000"

let yynames_const = "\
  IF\000\
  SEMICOLON\000\
  PARENTHESISOPEN\000\
  PARENTHESISCLOSE\000\
  COMMA\000\
  COLON\000\
  INVALID\000\
  ENDOFFILE\000\
  EOL\000\
  "

let yynames_block = "\
  FLOAT\000\
  INT\000\
  VARIABLE\000\
  STRING\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'clause1) in
    Obj.repr(
# 24 "parser.mly"
             ( _1 )
# 109 "parser.ml"
               : Backend.clause))
; (fun __caml_parser_env ->
    Obj.repr(
# 25 "parser.mly"
              ((Node(("EoF", 0), []), []))
# 115 "parser.ml"
               : Backend.clause))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'singleAtom) in
    Obj.repr(
# 30 "parser.mly"
             ( (_1,[]) )
# 122 "parser.ml"
               : 'clause1))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'singleAtom) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'body) in
    Obj.repr(
# 31 "parser.mly"
                        ( (_1, _3) )
# 130 "parser.ml"
               : 'clause1))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 35 "parser.mly"
         (Node((_1, 0), []) )
# 137 "parser.ml"
               : 'singleAtom))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'termlist) in
    Obj.repr(
# 36 "parser.mly"
                                                     (Node( (_1, List.length _3), _3))
# 145 "parser.ml"
               : 'singleAtom))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 39 "parser.mly"
           ( V(_1))
# 152 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'singleAtom) in
    Obj.repr(
# 40 "parser.mly"
               (_1)
# 159 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 43 "parser.mly"
              ( [_1] )
# 166 "parser.ml"
               : 'termlist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'termlist) in
    Obj.repr(
# 44 "parser.mly"
                          ( _1::_3 )
# 174 "parser.ml"
               : 'termlist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'singleAtom) in
    Obj.repr(
# 47 "parser.mly"
               (_1::[])
# 181 "parser.ml"
               : 'body))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'singleAtom) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'body) in
    Obj.repr(
# 48 "parser.mly"
                          (_1::_3)
# 189 "parser.ml"
               : 'body))
(* Entry main *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let main (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Backend.clause)
