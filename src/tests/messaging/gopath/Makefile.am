#-*-Mode:make;coding:utf-8;tab-width:4;c-basic-offset:4-*-
# ex: set ft=make fenc=utf-8 sts=4 ts=4 sw=4 noet nomod:

instdir = "$(DESTDIR)$(cloudi_prefix)/tests/messaging"
CLEANFILES = messaging_go

all-local: messaging_go

messaging_go: main.go
	GOBIN=$(abs_builddir) $(GO) install -x messaging_go

install-exec-hook: messaging_go
	$(MKDIR_P) $(instdir)
	$(INSTALL_SCRIPT) messaging_go $(instdir)

