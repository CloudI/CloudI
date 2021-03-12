#-*-Mode:make;coding:utf-8;tab-width:4;c-basic-offset:4-*-
# ex: set ft=make fenc=utf-8 sts=4 ts=4 sw=4 noet nomod:

instdir = "$(DESTDIR)$(cloudi_prefix)/tests/http_req"
CLEANFILES = http_req_go

all-local: http_req_go

http_req_go: main.go
	GOBIN=$(abs_builddir) $(GO) install -x http_req_go

install-exec-hook: http_req_go
	$(MKDIR_P) $(instdir)
	$(INSTALL_SCRIPT) http_req_go $(instdir)

