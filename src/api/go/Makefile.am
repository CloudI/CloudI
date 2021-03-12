#-*-Mode:make;coding:utf-8;tab-width:4;c-basic-offset:4-*-
# ex: set ft=make fenc=utf-8 sts=4 ts=4 sw=4 noet nomod:

instdir = "$(cloudi_prefix)/api/go"
directinstdir = "$(DESTDIR)$(instdir)"

install-exec-hook:
	$(MKDIR_P) $(directinstdir)
	$(MKDIR_P) $(directinstdir)/cloudi
	$(INSTALL_DATA) $(srcdir)/cloudi/go.mod \
                    $(directinstdir)/cloudi/
	$(INSTALL_DATA) $(srcdir)/cloudi/cloudi.go \
                    $(directinstdir)/cloudi/
	$(MKDIR_P) $(directinstdir)/erlang
	$(INSTALL_DATA) $(srcdir)/erlang/go.mod \
                    $(directinstdir)/erlang/
	$(INSTALL_DATA) $(srcdir)/erlang/erlang.go \
                    $(directinstdir)/erlang/

