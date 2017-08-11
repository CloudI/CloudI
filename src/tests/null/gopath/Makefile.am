#-*-Mode:make;coding:utf-8;tab-width:4;c-basic-offset:4-*-
# ex: set ft=make fenc=utf-8 sts=4 ts=4 sw=4 noet nomod:

instdir = "$(DESTDIR)$(cloudi_prefix)/tests/null"
CLEANFILES = null_go

all-local: null_go

null_go: src/null_go/main.go
	cp -rf $(srcdir)/src $(builddir) || exit 0
	$(MKDIR_P) $(builddir)/src/null_go/vendor
	rm -rf $(builddir)/src/null_go/vendor/cloudi
	cp -rf $(top_srcdir)/api/go/cloudi $(builddir)/src/null_go/vendor/
	rm -rf $(builddir)/src/null_go/vendor/erlang
	cp -rf $(top_srcdir)/api/go/erlang $(builddir)/src/null_go/vendor/
	$(MKDIR_P) $(builddir)/bin
	GOPATH=$(abs_builddir) GOBIN=$$GOPATH/bin $(GO) install -x null_go
	cp $(builddir)/bin/null_go .

clean-local:
	rm -rf $(builddir)/src/null_go/vendor/ \
           $(builddir)/pkg \
           $(builddir)/bin

install-exec-hook: null_go
	$(MKDIR_P) $(instdir)
	$(INSTALL_SCRIPT) null_go $(instdir)

