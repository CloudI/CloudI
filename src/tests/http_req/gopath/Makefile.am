#-*-Mode:make;coding:utf-8;tab-width:4;c-basic-offset:4-*-
# ex: set ft=make fenc=utf-8 sts=4 ts=4 sw=4 noet nomod:

instdir = "$(DESTDIR)$(cloudi_prefix)/tests/http_req/priv"

all-local: http_req_go

http_req_go:
	cp -rf $(srcdir)/src $(builddir) || exit 0
	$(MKDIR_P) $(builddir)/src/http_req_go/vendor
	cp -rf $(top_srcdir)/api/go/cloudi $(builddir)/src/http_req_go/vendor/
	cp -rf $(top_srcdir)/api/go/erlang $(builddir)/src/http_req_go/vendor/
	$(MKDIR_P) $(builddir)/bin
	GOPATH=$(abs_builddir) GOBIN=$$GOPATH/bin $(GO) install -x http_req_go
	cp $(builddir)/bin/http_req_go .

clean-local:
	rm -rf $(builddir)/src/http_req_go/vendor/
	rm -rf $(builddir)/pkg
	rm -rf $(builddir)/bin
	rm -f http_req_go

install-exec-hook: http_req_go
	$(MKDIR_P) $(instdir)
	$(INSTALL_SCRIPT) http_req_go $(instdir)

