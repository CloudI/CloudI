#-*-Mode:make;coding:utf-8;tab-width:4;c-basic-offset:4-*-
# ex: set ft=make fenc=utf-8 sts=4 ts=4 sw=4 noet nomod:

instdir = "$(cloudi_prefix)/tests/messaging/priv"
directinstdir = "$(DESTDIR)$(instdir)"
nativedir = "$(instdir)"
native_DATA = main.cmx
CLEANFILES = $(native_DATA) main.cmi main.o messaging_ocaml
OCAMLDEPS = nums.cmxa str.cmxa unix.cmxa threads/threads.cmxa \
            $(top_builddir)/api/ocaml/erlang.cmx \
            $(top_builddir)/api/ocaml/cloudi.cmx

all-local: $(native_DATA)
	$(OCAMLOPT) -o messaging_ocaml $(OCAMLDEPS) $(native_DATA)

.ml.cmx:
	$(OCAMLOPT) $(OCAMLFLAGS) -o $@ -c \
                -I $(top_builddir)/api/ocaml/ -thread $<

install-exec-hook: messaging_ocaml
	$(MKDIR_P) $(directinstdir)
	$(INSTALL_SCRIPT) messaging_ocaml $(directinstdir)

