#-*-Mode:make;coding:utf-8;tab-width:4;c-basic-offset:4-*-
# ex: set ft=make fenc=utf-8 sts=4 ts=4 sw=4 noet nomod:

instdir = "$(DESTDIR)$(cloudi_prefix)/tests/count"
CLEANFILES = count_go

all-local: count_go

count_go: main.go
	GOBIN=$(abs_builddir) $(GO) install -x count_go

install-exec-hook: count_go
	$(MKDIR_P) $(instdir)
	$(INSTALL_SCRIPT) count_go $(instdir)

