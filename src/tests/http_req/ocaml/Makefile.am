#-*-Mode:make;coding:utf-8;tab-width:4;c-basic-offset:4-*-
# ex: set ft=make fenc=utf-8 sts=4 ts=4 sw=4 noet nomod:

instdir = "$(DESTDIR)$(cloudi_prefix)/tests/http_req/priv"
nodist_noinst_SCRIPTS = http_req_ocaml

CLEANFILES = $(nodist_noinst_SCRIPTS) main.cmx main.cmi main.o
OCAMLDEPS = nums.cmxa str.cmxa unix.cmxa threads/threads.cmxa \
            $(top_builddir)/api/ocaml/erlang.cmx \
            $(top_builddir)/api/ocaml/cloudi.cmx

http_req_ocaml: main.cmx
	$(OCAMLOPT) -o http_req_ocaml $(OCAMLDEPS) main.cmx

.ml.cmx:
	$(OCAMLOPT) $(OCAMLFLAGS) -o $@ -c \
                -I $(top_builddir)/api/ocaml/ -thread $<

install-exec-hook: http_req_ocaml
	$(MKDIR_P) $(instdir)
	$(INSTALL_SCRIPT) http_req_ocaml $(instdir)

