#-*-Mode:make;coding:utf-8;tab-width:4;c-basic-offset:4-*-
# ex: set ft=make fenc=utf-8 sts=4 ts=4 sw=4 noet nomod:

instdir = "$(DESTDIR)$(cloudi_prefix)/tests/messaging"

all-local: messaging_go

messaging_go: src/messaging_go/main.go
	cp -rf $(srcdir)/src $(builddir) || exit 0
	$(MKDIR_P) $(builddir)/src/messaging_go/vendor
	rm -rf $(builddir)/src/messaging_go/vendor/cloudi
	cp -rf $(top_srcdir)/api/go/cloudi $(builddir)/src/messaging_go/vendor/
	rm -rf $(builddir)/src/messaging_go/vendor/erlang
	cp -rf $(top_srcdir)/api/go/erlang $(builddir)/src/messaging_go/vendor/
	$(MKDIR_P) $(builddir)/bin
	GOPATH=$(abs_builddir) GOBIN=$$GOPATH/bin $(GO) install -x messaging_go
	cp $(builddir)/bin/messaging_go .

clean-local:
	rm -rf $(builddir)/src/messaging_go/vendor/
	rm -rf $(builddir)/pkg
	rm -rf $(builddir)/bin
	rm -f messaging_go

install-exec-hook: messaging_go
	$(MKDIR_P) $(instdir)
	$(INSTALL_SCRIPT) messaging_go $(instdir)

