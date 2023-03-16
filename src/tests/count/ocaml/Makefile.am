#-*-Mode:make;coding:utf-8;tab-width:4;c-basic-offset:4-*-
# ex: set ft=make fenc=utf-8 sts=4 ts=4 sw=4 noet nomod:

instdir = "$(DESTDIR)$(cloudi_prefix)/tests/count"
CLEANFILES = count_ocaml main.cmx main.cmi main.o
OCAMLDEPS = str.cmxa unix.cmxa threads/threads.cmxa \
            $(top_builddir)/api/ocaml/external/zarith.cmxa \
            -ccopt -L$(top_builddir)/api/ocaml/external/ \
            $(top_builddir)/api/ocaml/cloudi.cmxa

all-local: count_ocaml

count_ocaml: main.mli main.ml
	$(OCAMLC) $(OCAMLFLAGS) -o main.cmi -c $(srcdir)/main.mli
	$(OCAMLOPT) $(OCAMLFLAGS) -o main.cmx -c \
                -I $(top_builddir)/api/ocaml/ -thread $(srcdir)/main.ml
	$(OCAMLOPT) -o count_ocaml $(OCAMLDEPS) main.cmx

install-exec-hook: count_ocaml
	$(MKDIR_P) $(instdir)
	$(INSTALL_SCRIPT) count_ocaml $(instdir)

