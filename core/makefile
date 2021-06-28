
.PHONY: doc

all: lis.cmo focus.cmo syntax.cmo

doc:
	mkdir -p doc
	ocamlfind ocamldoc -package yojson,ppx_deriving_yojson,lwt,js_of_ocaml,js_of_ocaml-lwt,js_of_ocaml-ppx -I webapp -html -d doc -colorize-code -charset utf-8 lis.ml focus.ml syntax.ml webapp/jsutils.ml webapp/html.ml webapp/webapp.ml webapp/widget_*.ml

clean:
	rm -f *.cm[ioax]

.SUFFIXES: .ml .mli .cmo .cmi

%.cmo: %.ml
	ocamlfind ocamlc -package yojson,ppx_deriving_yojson -c $<
