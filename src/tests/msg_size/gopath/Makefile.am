#-*-Mode:make;coding:utf-8;tab-width:4;c-basic-offset:4-*-
# ex: set ft=make fenc=utf-8 sts=4 ts=4 sw=4 noet nomod:

instdir = "$(DESTDIR)$(cloudi_prefix)/tests/msg_size"
CLEANFILES = msg_size_go

all-local: msg_size_go

msg_size_go: main.go
	GOBIN=$(abs_builddir) $(GO) install -x msg_size_go

install-exec-hook: msg_size_go
	$(MKDIR_P) $(instdir)
	$(INSTALL_SCRIPT) msg_size_go $(instdir)

