#-*-Mode:make;coding:utf-8;tab-width:4;c-basic-offset:4-*-
# ex: set ft=make fenc=utf-8 sts=4 ts=4 sw=4 noet nomod:

instdir = "$(DESTDIR)$(cloudi_prefix)/tests/count"
CLEANFILES = count_ocaml main.cmx main.cmi main.o
OCAMLDEPS = str.cmxa unix.cmxa threads/threads.cmxa \
            $(top_builddir)/api/ocaml/external/nums.cmxa \
            -ccopt -L$(top_builddir)/api/ocaml/external/ \
            $(top_builddir)/api/ocaml/cloudi.cmxa
SUFFIXES = .mli .ml .cmi .cmx

all-local: count_ocaml

count_ocaml: main.cmi main.cmx
	$(OCAMLOPT) -o count_ocaml $(OCAMLDEPS) main.cmx

.mli.cmi:
	$(OCAMLC) $(OCAMLFLAGS) -o $@ -c $<

.ml.cmx:
	$(OCAMLOPT) $(OCAMLFLAGS) -o $@ -c \
                -I $(top_builddir)/api/ocaml/ -thread $<

install-exec-hook: count_ocaml
	$(MKDIR_P) $(instdir)
	$(INSTALL_SCRIPT) count_ocaml $(instdir)

