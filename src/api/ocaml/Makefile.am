#-*-Mode:make;coding:utf-8;tab-width:4;c-basic-offset:4-*-
# ex: set ft=make fenc=utf-8 sts=4 ts=4 sw=4 noet nomod:

SUBDIRS = external
instdir = "$(cloudi_prefix)/api/ocaml"
directinstdir = "$(DESTDIR)$(instdir)"
nativedir = "$(instdir)"
native_DATA = erlang.cmi cloudi.cmi erlang.cmx cloudi.cmx
CLEANFILES = $(native_DATA) erlang.o cloudi.o
OCAMLDEPS = -I $(builddir)/external/

.NOTPARALLEL:

all-local: $(native_DATA)

.mli.cmi:
	$(OCAMLC) $(OCAMLFLAGS) -o $@ $(OCAMLDEPS) -c $<

.ml.cmx:
	$(OCAMLOPT) $(OCAMLFLAGS) -o $@ $(OCAMLDEPS) -c $<

install-exec-hook: $(native_DATA)
	$(MKDIR_P) $(directinstdir)
	$(INSTALL_DATA) erlang.cmi $(directinstdir)
	$(INSTALL_DATA) cloudi.cmi $(directinstdir)
	$(INSTALL_DATA) erlang.cmx $(directinstdir)
	$(INSTALL_DATA) cloudi.cmx $(directinstdir)

