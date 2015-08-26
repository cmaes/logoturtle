all: run

run: test.native sample_programs/tile.logo
	./test.native sample_programs/tile.logo

test.native: test.ml logoturtle.ml parser.mly lexer.mll
	ocamlbuild -use-menhir -tag thread -use-ocamlfind -quiet -pkg core -pkg cairo2 test.native

logoturtle.byte: logoturtle.ml
	ocamlfind ocamlc -linkpkg -thread -package cairo2 $< -o $@

graphics.png: logoturtle.byte
	-rm $@;
	./logoturtle.byte

clean:
	-rm *.cmo *.cmi graphics.png *.byte *.native
