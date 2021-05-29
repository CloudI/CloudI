#-*-Mode:make;coding:utf-8;tab-width:4;c-basic-offset:4-*-
# ex: set ft=make fenc=utf-8 sts=4 ts=4 sw=4 noet nomod:

instdir = "$(DESTDIR)$(cloudi_prefix)/tests/msg_size"
CLEANFILES = msg_size_ocaml main.cmx main.cmi main.o
OCAMLDEPS = str.cmxa unix.cmxa threads/threads.cmxa \
            $(top_builddir)/api/ocaml/external/nums.cmxa \
            -ccopt -L$(top_builddir)/api/ocaml/external/ \
            $(top_builddir)/api/ocaml/cloudi.cmxa

all-local: msg_size_ocaml

msg_size_ocaml: main.mli main.ml
	$(OCAMLC) $(OCAMLFLAGS) -o main.cmi -c main.mli
	$(OCAMLOPT) $(OCAMLFLAGS) -o main.cmx -c \
                -I $(top_builddir)/api/ocaml/ -thread main.ml
	$(OCAMLOPT) -o msg_size_ocaml $(OCAMLDEPS) main.cmx

install-exec-hook: msg_size_ocaml
	$(MKDIR_P) $(instdir)
	$(INSTALL_SCRIPT) msg_size_ocaml $(instdir)

