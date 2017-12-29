#-*-Mode:make;coding:utf-8;tab-width:4;c-basic-offset:4-*-
# ex: set ft=make fenc=utf-8 sts=4 ts=4 sw=4 noet nomod:

instdir = "$(DESTDIR)$(cloudi_prefix)/tests/null"
CLEANFILES = null_ocaml main.cmx main.cmi main.o
OCAMLDEPS = str.cmxa unix.cmxa threads/threads.cmxa \
            $(top_builddir)/api/ocaml/external/nums.cmxa \
            -ccopt -L$(top_builddir)/api/ocaml/external/ \
            $(top_builddir)/api/ocaml/erlang.cmx \
            $(top_builddir)/api/ocaml/cloudi.cmx
SUFFIXES = .ml .cmx

all-local: null_ocaml

null_ocaml: main.cmx
	$(OCAMLOPT) -o null_ocaml $(OCAMLDEPS) main.cmx

.ml.cmx:
	$(OCAMLOPT) $(OCAMLFLAGS) -o $@ -c \
                -I $(top_builddir)/api/ocaml/ -thread $<

install-exec-hook: null_ocaml
	$(MKDIR_P) $(instdir)
	$(INSTALL_SCRIPT) null_ocaml $(instdir)

