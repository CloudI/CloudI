#-*-Mode:make;coding:utf-8;tab-width:4;c-basic-offset:4-*-
# ex: set ft=make fenc=utf-8 sts=4 ts=4 sw=4 noet nomod:

instdir = "$(DESTDIR)$(cloudi_prefix)/tests/msg_size/priv"

all-local:
	cp -rf $(srcdir)/src $(builddir) || exit 0
	$(MKDIR_P) $(builddir)/src/msg_size_go/vendor
	cp -rf $(top_srcdir)/api/go/cloudi $(builddir)/src/msg_size_go/vendor/
	cp -rf $(top_srcdir)/api/go/erlang $(builddir)/src/msg_size_go/vendor/
	$(MKDIR_P) $(builddir)/bin
	GOPATH=$(abs_builddir) GOBIN=$$GOPATH/bin $(GO) install -x msg_size_go

clean-local:
	rm -rf $(builddir)/src/msg_size_go/vendor/
	rm -rf $(builddir)/pkg
	rm -rf $(builddir)/bin

install-exec-hook: $(builddir)/bin/msg_size_go
	$(MKDIR_P) $(instdir)
	$(INSTALL_SCRIPT) $(builddir)/bin/msg_size_go $(instdir)

