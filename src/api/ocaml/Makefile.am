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
              cloudi.cmxa
CLEANFILES = $(native_DATA) \
             erlang.cmo \
             erlang.o \
             cloudi.cmo \
             cloudi.a \
             cloudi.o
OCAMLDEPS = -I $(builddir)/external/
SUFFIXES = .mli .ml .cmi .cmx .cmo

.NOTPARALLEL:

all-local: $(native_DATA)

cloudi.cmxa: erlang.cmx erlang.cmi cloudi.cmx cloudi.cmi
	$(OCAMLOPT) $(OCAMLFLAGS) -a erlang.cmx cloudi.cmx -o $@

cloudi.cma: erlang.cmo erlang.cmi cloudi.cmo cloudi.cmi
	$(OCAMLC) $(OCAMLFLAGS) -a erlang.cmo cloudi.cmo -o $@

.mli.cmi:
	$(OCAMLC) $(OCAMLFLAGS) -o $@ $(OCAMLDEPS) -c $<

.ml.cmx:
	$(OCAMLOPT) $(OCAMLFLAGS) -o $@ $(OCAMLDEPS) -c $<

.ml.cmo:
	$(OCAMLC) $(OCAMLFLAGS) -o $@ $(OCAMLDEPS) -c $<

install-exec-hook: $(native_DATA) cloudi.a
	$(MKDIR_P) $(directinstdir)
	$(INSTALL_DATA) erlang.cmi $(directinstdir)
	$(INSTALL_DATA) erlang.cmx $(directinstdir)
	$(INSTALL_DATA) cloudi.cmi $(directinstdir)
	$(INSTALL_DATA) cloudi.cmx $(directinstdir)
	$(INSTALL_DATA) cloudi.a $(directinstdir)
	$(INSTALL_DATA) cloudi.cma $(directinstdir)
	$(INSTALL_DATA) cloudi.cmxa $(directinstdir)

