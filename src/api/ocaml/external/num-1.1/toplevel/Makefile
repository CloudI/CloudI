OCAMLC=ocamlc
OCAMLDEP=ocamldep
OCAMLFIND=ocamlfind

CAMLCFLAGS=-I ../src -I +compiler-libs \
           -w +a-4-9-41-42-44-45-48 -warn-error A \
           -safe-string -strict-sequence -strict-formats

CMOS=num_top_printers.cmo num_top.cmo

all: num_top.cma

num_top.cma: $(CMOS)
	$(OCAMLC) $(CAMLCFLAGS) -a -o $@ $(CMOS)

%.cmi: %.mli
	$(OCAMLC) $(CAMLCFLAGS) -c $*.mli
%.cmo: %.ml
	$(OCAMLC) $(CAMLCFLAGS) -c $*.ml

TOINSTALL=\
  num_top.cma num_top.cmi num_top_printers.cmi

install:
	$(OCAMLFIND) install num-top META $(TOINSTALL)

uninstall:
	$(OCAMLFIND) remove num-top

clean:
	rm -f *.cm[ioxta] *.cmx[as] *.cmti

depend:
	$(OCAMLDEP) -slash *.mli *.ml > .depend

include .depend
