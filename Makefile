play:
	ocamlbuild -use-ocamlfind main.byte && ./main.byte

test:
	ocamlbuild -use-ocamlfind test.byte && ./test.byte
