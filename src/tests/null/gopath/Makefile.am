#-*-Mode:make;coding:utf-8;tab-width:4;c-basic-offset:4-*-
# ex: set ft=make fenc=utf-8 sts=4 ts=4 sw=4 noet nomod:

instdir = "$(DESTDIR)$(cloudi_prefix)/tests/null/priv"

all-local:
	cp -rf $(srcdir)/src $(builddir) || exit 0
	$(MKDIR_P) $(builddir)/src/null_go/vendor
	cp -rf $(top_srcdir)/api/go/cloudi $(builddir)/src/null_go/vendor/
	cp -rf $(top_srcdir)/api/go/erlang $(builddir)/src/null_go/vendor/
	$(MKDIR_P) $(builddir)/bin
	GOPATH=$(abs_builddir) GOBIN=$$GOPATH/bin go install -x null_go

clean-local:
	rm -rf $(builddir)/src/null_go/vendor/
	rm -rf $(builddir)/pkg
	rm -rf $(builddir)/bin

install-exec-hook: $(builddir)/bin/null_go
	$(MKDIR_P) $(instdir)
	$(INSTALL_SCRIPT) $(builddir)/bin/null_go $(instdir)

