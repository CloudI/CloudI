#-*-Mode:make;coding:utf-8;tab-width:4;c-basic-offset:4-*-
# ex: set ft=make fenc=utf-8 sts=4 ts=4 sw=4 noet nomod:

instdir = "$(DESTDIR)$(cloudi_prefix)/tests/null"
CLEANFILES = null_go

all-local: null_go

null_go: main.go
	GOBIN=$(abs_builddir) $(GO) install -x null_go

install-exec-hook: null_go
	$(MKDIR_P) $(instdir)
	$(INSTALL_SCRIPT) null_go $(instdir)

