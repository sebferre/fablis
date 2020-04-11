
all: focus.cmo syntax.cmo lis.cmo

clean:
	rm -f *.cm[ioax]

.SUFFIXES: .ml .mli .cmo .cmi

%.cmo: %.ml
	ocamlfind ocamlc -package yojson,ppx_deriving_yojson -c $<
