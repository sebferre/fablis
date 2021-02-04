
all: jsutils.cmo webapp.cmo html.cmo widget_focus.cmo widget_suggestions.cmo widget_table.cmo widget_commandline.cmo

clean:
	rm -f *.cm[ioax]

.SUFFIXES: .ml .mli .cmo .cmi

%.cmo: %.ml
	ocamlfind ocamlc -I ../../core -package yojson,lwt,js_of_ocaml,js_of_ocaml-lwt,js_of_ocaml-ppx -c $<
