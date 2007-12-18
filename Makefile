all: patchdep
include .dep

.SUFFIXES: .mli .cmi
.SUFFIXES: .ml .cmx
.SUFFIXES: .mll .ml
.SUFFIXES: .mly .ml

.mli.cmi:
	ocamlopt -c $<

.ml.cmx: 
	ocamlopt $(CFLAGS) -c $(INCLUDEDIR) $<

.mly.ml: 
	ocamlyacc $< 

.mll.ml:
	ocamllex $< 

patchdep: types.cmx parser.mli parser.cmi parser.cmx lexer.cmx pretty.cmx helpers.cmx dependencies.cmx main.cmx 
	ocamlopt types.cmx unix.cmxa parser.cmx lexer.cmx pretty.cmx helpers.cmx dependencies.cmx main.cmx -o patchdep

dep:
	ocamldep *.ml > .dep

clean:
	rm -f *.cmi *.cmx
