#-*-Mode:make;coding:utf-8;tab-width:4;c-basic-offset:4-*-
# ex: set ft=make fenc=utf-8 sts=4 ts=4 sw=4 noet nomod:

SUBDIRS = external
instdir = "$(cloudi_prefix)/api/ocaml"
directinstdir = "$(DESTDIR)$(instdir)"
nativedir = "$(instdir)"
native_DATA = erlang.cmi \
              erlang.cmx \
              cloudi.cmi \
              cloudi.cmx \
              cloudi.cma \
              cloudi.cmxa \
			  cloudi.a
CLEANFILES = $(native_DATA) \
             erlang.cmo \
             erlang.o \
             cloudi.cmo \
             cloudi.o
OCAMLDEPS = -I $(builddir)/external/
SUFFIXES = .mli .ml .cmi .cmx .cmo

.NOTPARALLEL:

all-local: $(native_DATA)

cloudi.cmxa: erlang.cmx erlang.cmi cloudi.cmx cloudi.cmi
	$(OCAMLOPT) $(OCAMLFLAGS) -a erlang.cmx cloudi.cmx -o $@

cloudi.cma: erlang.cmo erlang.cmi cloudi.cmo cloudi.cmi
	$(OCAMLC) $(OCAMLFLAGS) -a erlang.cmo cloudi.cmo -o $@

cloudi.a: cloudi.cmxa

.mli.cmi:
	$(OCAMLC) $(OCAMLFLAGS) -o $@ $(OCAMLDEPS) -c $<

.ml.cmx:
	$(OCAMLOPT) $(OCAMLFLAGS) -o $@ $(OCAMLDEPS) -c $<

.ml.cmo:
	$(OCAMLC) $(OCAMLFLAGS) -o $@ $(OCAMLDEPS) -c $<

