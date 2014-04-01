all:
	ocamlbuild -lib str gen-parser.native

junk:
	ocamlopt -c abnf_syntaxtree.ml
	ocamlopt -c abnf_location.ml
	ocamlopt -c abnf_ops.ml str.cmxa
	ocamlopt -c abnf_parser.ml
	ocamlopt -c abnf_lexer.ml
	ocamlopt -o gen_types gen_types.ml str.cmxa abnf_syntaxtree.o abnf_location.o abnf_ops.o abnf_parser.o abnf_lexer.o
clean:
	rm gen_types *.cmi *.cmo
