all:
	ocamllex lexer.mll       # generates lexer.ml
	ocamlyacc parser.mly     # generates parser.ml and parser.mli
# 	ocamlc -c parser.mli
# 	ocamlc -c lexer.ml
# 	ocamlc -c backend.ml
# 	ocamlc -c parser.ml
# 	ocamlc -c main.ml
	ocamlc -o assignment4 str.cma parser.mli lexer.ml backend.ml parser.ml main.ml
# 	ocamlc -o main backend.cmo lexer.cmo parser.cmo main.cmo
clean:
	rm -rf assignment4 *.cmi *.cmo *.mli lexer.ml parser.ml lexer.ml

run:
	./assignment4 sheet.csv 15 15 instructions.txt > output.txt
