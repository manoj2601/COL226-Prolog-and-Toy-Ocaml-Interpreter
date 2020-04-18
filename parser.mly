%{	(*Here we initilize all the functions which may use in the next assignment. For our convenience,
assume that return type of these functions in int *)
open Backend
exception InvalidInput
exception Error


(*Sheet initilization*)

(* Sheet dimensions *)
let m = int_of_string Sys.argv.(2);; (*Number of rows*)
let n = int_of_string Sys.argv.(3);;	(*Number of cols*)
let arr = Array.make_matrix m n None;;	(*Create a matrix of size m*n with all None values*)

let rec print_list = function (*It prints a list*)
	[] -> ()
	| e::l -> print_string e ; print_string " "; print_list l
;;


(* fill_sheet takes the arguments arr (sheet), a list that is to be stored in the sheet,
row that indicates the row number in which the list is supposed to fill, col that is the starting index of that row where the first element of the list should be placed
*)
let rec fill_sheet (arr:sheet) (l:string list) (row:int) (col:int) =
	match l with
	[] ->()
	| x::xs ->
		if (x <> "") then arr.(row).(col) <- (Some (float_of_string x))
		else arr.(row).(col) <- None;
		fill_sheet arr xs row (col+1);
;;

(*This takes the cvs file as argument and split it with ',' and create a string list values, then we call the function fill_sheet to fill values in the sheet *)
let _ =
  try
    let in_stream = open_in Sys.argv.(1) in
        for i=0 to (m-1) do
          let line = input_line in_stream in
          let split = Str.split (Str.regexp ",") in
          let values = split line in
            print_list values;
            fill_sheet arr values i 0;
            Printf.printf "\n";
        done;
        close_in in_stream; 
  with e ->
    Printf.printf "File not found!";
    raise e


%}
	/*UPDATE : 8 MARCH 2020
	Here are all the tokens defined that are in ocamllex file
	There is a slight changes from last assignment in tokens.
	1) FLOAT : All the integers are also FLOAT
	2) INDEX : Maintained whitespaces 
		Previous : [3,5] was valid
		Now : [3, 5] is valid
	3) RANGE : Maintained whitespaces
		Previous : ([1,2]:[3,4]) was valid
		Now : ( [1, 2] : [3, 4] ) is valid
	4) A new token EOL for end of line created.
	*/
  %token <float> FLOAT
  %token <int> INT
  %token <int*int> INDEX
  %token <int*int*int*int>RANGE
  %token SEMICOLON
  %token ASSIGN
  %token PARENTHESISOPEN
  %token PARENTHESISCLOSE
  %token BRACKETSOPEN
  %token BRACKETSCLOSE
  %token COMMA
  %token COUNT
  %token COLON
  %token SUM
  %token AVG
  %token MIN
  %token MAX
  %token ROWCOUNT
  %token COLCOUNT
  %token ROWSUM
  %token COLSUM
  %token ROWAVG
  %token COLAVG
  %token ROWMIN
  %token COLMIN
  %token ROWMAX
  %token COLMAX
  %token ADD
  %token SUBT
  %token MULT
  %token DIV
  %token INVALID
  %token ENDOFFILE
  %token EOL
%start main
%type <unit> main

%%
main:
  expr EOL{print_sheet $1; ()}		/*If the input is of valid type then expr will return the updated arr as $1. and main will return an integer value 1.*/
  | error EOL {Printf.printf "INVALID INPUT\n" ; ()} /*If the input is not of expr type then return -1*/
;
 /*Here all the valid type of input formats are listed. */
expr:
  INDEX ASSIGN ADD RANGE RANGE SEMICOLON {Printf.printf "calling add_range\n"; (add_range arr $4 $5 $1)}
  | INDEX ASSIGN ADD FLOAT RANGE SEMICOLON {Printf.printf "calling add_const\n";  add_const arr $5 $4 $1}
  | INDEX ASSIGN ADD RANGE FLOAT SEMICOLON {Printf.printf "calling add_const\n";  add_const arr $4 $5 $1}
  | INDEX ASSIGN ADD RANGE INDEX SEMICOLON {Printf.printf "calling add_range2\n";  add_range2 arr $4 $5 $1}
  | INDEX ASSIGN ADD INDEX RANGE SEMICOLON {Printf.printf "calling add_range2\n";  add_range2 arr $5 $4 $1}
  | INDEX ASSIGN SUBT RANGE RANGE SEMICOLON {Printf.printf "calling subt_range\n";  subt_range arr $4 $5 $1}
  | INDEX ASSIGN SUBT FLOAT RANGE SEMICOLON {Printf.printf "calling subt_const\n";  subt_const arr $5 $4 $1}
  | INDEX ASSIGN SUBT RANGE FLOAT SEMICOLON {Printf.printf "calling subt_const\n";  subt_const arr $4 $5 $1}
  | INDEX ASSIGN SUBT RANGE INDEX SEMICOLON {Printf.printf "calling subt_range2\n"; subt_range2 arr $4 $5 $1}
  | INDEX ASSIGN SUBT INDEX RANGE SEMICOLON {Printf.printf "calling subt_range2\n";  subt_range2 arr $5 $4 $1}
  | INDEX ASSIGN MULT RANGE RANGE SEMICOLON {Printf.printf "calling mult_range\n";  mult_range arr $4 $5 $1}
  | INDEX ASSIGN MULT FLOAT RANGE SEMICOLON {Printf.printf "calling mult_const\n";  mult_const arr $5 $4 $1}
  | INDEX ASSIGN MULT RANGE FLOAT SEMICOLON {Printf.printf "calling mult_const\n";  mult_const arr $4 $5 $1}
  | INDEX ASSIGN MULT RANGE INDEX SEMICOLON {Printf.printf "calling mult_range2\n";  mult_range2 arr $4 $5 $1}
  | INDEX ASSIGN MULT INDEX RANGE SEMICOLON {Printf.printf "calling mult_range2\n";  mult_range2 arr $5 $4 $1}
  | INDEX ASSIGN DIV RANGE RANGE SEMICOLON {Printf.printf "calling div_range\n";  div_range arr $4 $5 $1}
  | INDEX ASSIGN DIV FLOAT RANGE SEMICOLON {Printf.printf "calling div_const\n";  div_const arr $5 $4 $1}
  | INDEX ASSIGN DIV RANGE FLOAT SEMICOLON {Printf.printf "calling div_const\n";  div_const arr $4 $5 $1}
  | INDEX ASSIGN DIV RANGE INDEX SEMICOLON {Printf.printf "calling div_range2\n";  div_range2 arr $4 $5 $1}
  | INDEX ASSIGN DIV INDEX RANGE SEMICOLON {Printf.printf "calling div_range2\n";  div_range2 arr $5 $4 $1}
  | INDEX ASSIGN COUNT RANGE SEMICOLON      {Printf.printf "calling full_count\n";  full_count arr $4 $1}
  | INDEX ASSIGN ROWCOUNT RANGE SEMICOLON   {Printf.printf "calling row_count\n";  row_count arr $4 $1}
  | INDEX ASSIGN COLCOUNT RANGE SEMICOLON   {Printf.printf "calling col_count\n";  col_count arr $4 $1}
  | INDEX ASSIGN SUM RANGE SEMICOLON   {Printf.printf "calling full_sum\n";  full_sum arr $4 $1}
  | INDEX ASSIGN ROWSUM RANGE SEMICOLON   {Printf.printf "calling row_sum\n";  row_sum arr $4 $1}
  | INDEX ASSIGN COLSUM RANGE SEMICOLON   {Printf.printf "calling col_sum\n";  col_sum arr $4 $1}
  | INDEX ASSIGN AVG RANGE SEMICOLON   {Printf.printf "calling full_avg\n";  full_avg arr $4 $1}
  | INDEX ASSIGN ROWAVG RANGE SEMICOLON   {Printf.printf "calling row_avg\n";  row_avg arr $4 $1}
  | INDEX ASSIGN COLAVG RANGE SEMICOLON   {Printf.printf "calling col_avg\n";  col_avg arr $4 $1}
  | INDEX ASSIGN MIN RANGE SEMICOLON   {Printf.printf "calling full_min\n";  full_min arr $4 $1}
  | INDEX ASSIGN ROWMIN RANGE SEMICOLON   {Printf.printf "calling row_min\n";  row_min arr $4 $1}
  | INDEX ASSIGN COLMIN RANGE SEMICOLON   {Printf.printf "calling col_min\n";  col_min arr $4 $1}
  | INDEX ASSIGN MAX RANGE SEMICOLON   {Printf.printf "calling full_max\n";  full_max arr $4 $1}
  | INDEX ASSIGN ROWMAX RANGE SEMICOLON   {Printf.printf "calling row_max\n";  row_max arr $4 $1}
  | INDEX ASSIGN COLMAX RANGE SEMICOLON   {Printf.printf "calling col_max\n";  col_max arr $4 $1}
  ;

%%
