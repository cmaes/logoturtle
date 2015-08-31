all: run

run: logo.native sample_programs/tree.logo
	./logo.native sample_programs/tree.logo

logo.native: logo.ml logoturtle.ml parser.mly lexer.mll
	ocamlbuild -use-menhir -tag thread -use-ocamlfind -quiet -pkg core -pkg cairo2 $@

logoturtle.byte: logoturtle.ml
	ocamlfind ocamlc -linkpkg -thread -package cairo2 $< -o $@

graphics.png: logoturtle.byte
	-rm $@;
	./logoturtle.byte

clean:
	-rm *.cmo *.cmi graphics.png *.byte *.native
	-rm -rf _build/
