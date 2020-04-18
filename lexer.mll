{

	open Parser
	exception Eof ;;
(*type token = (**)
	FLOAT of float
	| INT of int
	| INDEX of int*int
	| RANGE of int*int*int*int
	| SEMICOLON
	| ASSIGN
	| PARENTHESISOPEN
	| PARENTHESISCLOSE
	| BRACKETSOPEN
	| BRACKETSCLOSE
	| COMMA
	| COUNT
	| COLON
	| SUM
	| AVG
	| MIN
	| MAX
	| ROWCOUNT
	| COLCOUNT
	| ROWSUM
	| COLSUM
	| ROWAVG
	| COLAVG
	| ROWMIN
	| COLMIN
	| ROWMAX
	| COLMAX
	| ADD
	| SUBT
	| MULT
	| DIV
	| INVALID
	| ENDOFFILE*)
}

let DIGIT = ['0'-'9']

rule token = parse
| ';'	{SEMICOLON}
| ':''=' {ASSIGN}
|  '('	{PARENTHESISOPEN}
| ')'	{PARENTHESISCLOSE}
| '['	{BRACKETSOPEN}
| ']'	{BRACKETSCLOSE}
| ','	{COMMA}
| [' ' '\t']+ {token lexbuf}
| "COUNT" {COUNT}
| ('+'|'-')?(['0']|['1'-'9']['0'-'9']*)('.'['0'-'9']+)? as flt {FLOAT(float_of_string flt)}
| ('+'|'-')?(['0']|['1'-'9']['0'-'9']*) as flt {INT(int_of_string flt)}
| ':'	{COLON}
(*UNARY OPERATIONS START*)
| "SUM"		{SUM}
| "AVG" 	{AVG}
| "MIN"		{MIN}
| "MAX"		{MAX}
| "ROWCOUNT"	{ROWCOUNT}
| "COLCOUNT"	{COLCOUNT}
| "ROWSUM"	{ROWSUM}
| "COLSUM"	{COLSUM}
| "ROWAVG"	{ROWAVG}
| "COLAVG"	{COLAVG}
| "ROWMIN"	{ROWMIN}
| "COLMIN"	{COLMIN}
| "ROWMAX"	{ROWMAX}
| "COLMAX"	{COLMAX}
(*UNARY OPERATIONS END*)
(*BINARY OPERATIONS START*)
| "ADD"		{ADD}
| "SUBT"	{SUBT}
| "MULT"	{MULT}
| "DIV"		{DIV}
(*BINARY OPERATIONS END*)
(*INDEX and RANGE*)
| '['[' ']* ((['0']|(['1'-'9']['0'-'9']*)) as a1) [' ']*","[' ']* (['0']|(['1'-'9']['0'-'9']*) as a2) [' ']*']'[' ']* {INDEX(int_of_string a1, int_of_string a2)}
| "("[' ']* '['((['0']|(['1'-'9']['0'-'9']*)) as a1) [' ']*","[' ']* (['0']|(['1'-'9']['0'-'9']*) as a2) [' ']*']' [' ']*":"[' ']* '['((['0']|(['1'-'9']['0'-'9']*)) as a3) [' ']*","[' ']* (['0']|(['1'-'9']['0'-'9']*) as a4) [' ']*']'[' ']* ")" 	{RANGE(int_of_string a1, int_of_string a2, int_of_string a3, int_of_string a4)}
| eof {raise Eof}
| '\n' {EOL}
| _ {INVALID}



(*0 is considered as both FLOAT and INT
0.0 is considered as FLOAT
02 is invalid token
00.02 is invalid token
*)

(*
UPDATE : 17 March 2020
Before : INDEX : Entries like [0, 3] were valid, entries like [0,3] and [  0 ,   3   ] were not valid
		 RANGE : Entries like ( [0, 3] : [4, 3] ) were valid. i.e. exact number of spaces on each position was necessary.
NOW : INDEX and RANGE can contain any number of spaces anywhere in the expression.
*)