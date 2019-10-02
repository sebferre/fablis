
all: jsutils.cmo webapp.cmo html.cmo widget_focus.cmo

clean:
	rm -f *.cm[ioax]

.SUFFIXES: .ml .mli .cmo .cmi

%.cmo: %.ml
	ocamlfind ocamlc -I ../../core -package js_of_ocaml -package js_of_ocaml.syntax -syntax camlp4o -c $<
