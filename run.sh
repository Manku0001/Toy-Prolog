ocamlc -c assignment6.ml
	ocamllex lexer.mll
	ocamlyacc parser.mly
	ocamlc -c parser.mli
	ocamlc -c lexer.ml
	ocamlc -c parser.ml
	ocamlc -c toyprolog.ml
ocamlc -o toyprolog assignment6.cmo lexer.cmo parser.cmo toyprolog.cmo 