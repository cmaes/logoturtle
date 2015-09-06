all: run

TEST_PROGRAMS := $(wildcard sample_programs/*.logo)
TEST_GRAPHICS = $(TEST_PROGRAMS:.logo=.png)

test: $(TEST_GRAPHICS)

%.png: %.logo logo.native
	./logo.native $<

run: logo.native sample_programs/tree.logo
	./logo.native sample_programs/tree.logo

logo.native: logo.ml logoturtle.ml turtlegraphics.ml parser.mly lexer.mll
	ocamlbuild -use-menhir -tag thread -use-ocamlfind -quiet -pkg core -pkg cairo2 $@

# logoturtle.byte: logoturtle.ml
# 	ocamlfind ocamlc -linkpkg -thread -package cairo2 $< -o $@

lexer.ml: lexer.mll
	ocamllex lexer.mll

parser.ml parser.mli: parser.mly
	menhir parser.mly

turtlegraphics.cmo: turtlegraphics.ml
	 ocamlfind ocamlc -package js_of_ocaml -package js_of_ocaml.syntax -syntax camlp4o -linkpkg -o turtlegraphics.byte turtlegraphics.ml

logoturtle.cmo: logoturtle.ml turtlegraphics.cmo
	ocamlc -c logoturtle.ml

parser.cmo: parser.mli parser.ml logoturtle.cmo
	ocamlc -c parser.mli
	ocamlc -c parser.ml

lexer.cmo: lexer.ml parser.cmo
	ocamlc -c lexer.ml

logoturtle.byte: logoturtle.ml lexer.cmo parser.cmo turtlegraphics.cmo
	ocamlfind ocamlc -package js_of_ocaml -package js_of_ocaml.syntax -syntax camlp4o -linkpkg turtlegraphics.cmo logoturtle.ml -o logoturtle.byte

logoweb.byte: logoweb.ml logoturtle.cmo parser.cmo lexer.cmo
	ocamlfind ocamlc -package js_of_ocaml -package js_of_ocaml.syntax -syntax camlp4o -linkpkg turtlegraphics.cmo logoturtle.cmo lexer.cmo parser.cmo logoweb.ml -o logoweb.byte

logoweb.js: logoweb.byte
	js_of_ocaml --pretty --no-inline --debug-info logoweb.byte

graphics.png: logoturtle.byte
	-rm $@;
	./logoturtle.byte

clean:
	-rm *.cmo *.cmi *.png *.byte *.native parser.mli parser.ml lexer.ml logoweb.js
	-rm -rf _build/
