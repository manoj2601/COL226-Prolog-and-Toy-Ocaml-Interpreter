%{	(*Here we initilize all the functions which may use in the next assignment. For our convenience,
assume that return type of these functions in int *)
open Backend
exception InvalidInput
exception Error

(* let rec FormingTable l lexbuf =
  let input = Parser.main Lexer.token lexbuf in
  match input with
  | Node ( ( ("Eof", 0), []), []) -> l
  | _ -> FormingTable (input::l)
;; *)



(* let table = (*Forming table*)
  try
    let in_stream = open_in Sys.argv.(1) in
    let lexbuf = Lexing.from_channel in_stream in
    let ret = FormingTable [] lexbuf in
    close_in in_stream;
    ret
  with e ->
    Printf.printf "File Not found!";
    raise e *)
%}

  %token <float> FLOAT
  %token <int> INT
  %token IF
  %token <string> VARIABLE
  %token <string> STRING
  %token SEMICOLON
  %token PARENTHESISOPEN
  %token PARENTHESISCLOSE
  %token COMMA
  %token COLON
  %token INVALID
  %token ENDOFFILE
  %token EOL
%start main
%type <Backend.clause> main
%%
main:
  clause1 EOL{ $1 }		/*If the input is of valid type then expr will return the updated arr as $1. and main will return an integer value 1.*/
  | ENDOFFILE {(Node(("file_end", 0), []), [])}
;
 /*Here all the valid type of input formats are listed. */

clause1:
  singleAtom { ($1,[]) } /*fact*/
  | singleAtom IF body  { ($1, $3) } /*Rule*/
  ;

singleAtom:
  STRING {Node(($1, 0), []) } /*zero arity*/ 
  | STRING PARENTHESISOPEN termlist PARENTHESISCLOSE {Node( ($1, List.length $3), $3)} /*one or more arity*/
  ;
term:
  VARIABLE { V($1)}
  | singleAtom {$1}
  ;
termlist:
     term     { [$1] }
    | term COMMA termlist { $1::$3 }
;
body:
  | singleAtom {$1::[]}
  | singleAtom COMMA body {$1::$3}
  ;