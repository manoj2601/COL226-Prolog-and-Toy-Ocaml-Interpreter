all:
	ocamlc -c backend.ml
	ocamllex lexer.mll
	ocamlyacc parser.mly
	ocamlc -c parser.mli
	ocamlc -c lexer.ml
	ocamlc -c parser.ml
	ocamlc -c main.ml
	ocamlc -o swipl backend.cmo lexer.cmo parser.cmo main.cmo 
	
clean:
	rm swipl *.cmo *.cmi *.mli lexer.ml parser.ml
run:
	./swipl input.pl
