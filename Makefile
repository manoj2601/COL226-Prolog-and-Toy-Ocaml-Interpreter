all:
	ocamlc -c backend.ml
	ocamllex lexer.mll
	ocamlyacc parser.mly
	ocamlc -c parser.mli
	ocamlc -c lexer.ml
	ocamlc -c parser.ml
	ocamlc -c main.ml
	ocamlc -o toyprolog backend.cmo  lexer.cmo parser.cmo main.cmo 
	
clean:
	rm toyprolog *.cmo *.cmi *.mli lexer.ml parser.ml
run:
	./toyprolog input.pl
