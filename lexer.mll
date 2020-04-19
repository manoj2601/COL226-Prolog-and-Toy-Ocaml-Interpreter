{

	open Parser
	exception Eof ;;
}

let DIGIT = ['0'-'9']
let Capital = ['A'-'Z''_']
let string = ['a'-'z']['A'-'Z''a'-'z''_']*

rule token = parse
| ';'	{SEMICOLON}
| ':''-' {IF}
|  '('	{PARENTHESISOPEN}
| ')'	{PARENTHESISCLOSE}
| ','	{COMMA}
| [' ' '\t''\n']+ {token lexbuf}
| (Capital)(string*) as str {VARIABLE(str)}
| (string*) as str {STRING (str)}
| ':'	{COLON}
| eof {raise Eof}
| '.'('\n')? {EOL}
| ('+'|'-')?(['0']|['1'-'9']['0'-'9']*)('.'['0'-'9']+)? as flt {FLOAT(float_of_string flt)}
| ('+'|'-')?(['0']|['1'-'9']['0'-'9']*) as flt {INT(int_of_string flt)}
| _ {INVALID}


