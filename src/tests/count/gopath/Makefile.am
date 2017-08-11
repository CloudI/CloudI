#-*-Mode:make;coding:utf-8;tab-width:4;c-basic-offset:4-*-
# ex: set ft=make fenc=utf-8 sts=4 ts=4 sw=4 noet nomod:

instdir = "$(DESTDIR)$(cloudi_prefix)/tests/count"

all-local: count_go

count_go: src/count_go/main.go
	cp -rf $(srcdir)/src $(builddir) || exit 0
	$(MKDIR_P) $(builddir)/src/count_go/vendor
	rm -rf $(builddir)/src/count_go/vendor/cloudi
	cp -rf $(top_srcdir)/api/go/cloudi $(builddir)/src/count_go/vendor/
	rm -rf $(builddir)/src/count_go/vendor/erlang
	cp -rf $(top_srcdir)/api/go/erlang $(builddir)/src/count_go/vendor/
	$(MKDIR_P) $(builddir)/bin
	GOPATH=$(abs_builddir) GOBIN=$$GOPATH/bin $(GO) install -x count_go
	cp $(builddir)/bin/count_go .

clean-local:
	rm -rf $(builddir)/src/count_go/vendor/
	rm -rf $(builddir)/pkg
	rm -rf $(builddir)/bin
	rm -f count_go

install-exec-hook: count_go
	$(MKDIR_P) $(instdir)
	$(INSTALL_SCRIPT) count_go $(instdir)

