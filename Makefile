all: graphics.png


logoturtle.byte: logoturtle.ml
	ocamlfind ocamlc -linkpkg -thread -package cairo2 $< -o $@

graphics.png: logoturtle.byte
	-rm $@;
	./logoturtle.byte

clean:
	-rm *.cmo *.cmi graphics.png *.byte
