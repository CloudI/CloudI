#-*-Mode:make;coding:utf-8;tab-width:4;c-basic-offset:4-*-
# ex: set ft=make fenc=utf-8 sts=4 ts=4 sw=4 noet nomod:

instdir = "$(DESTDIR)$(cloudi_prefix)/tests/msg_size/priv"

all-local: msg_size_go

msg_size_go: src/msg_size_go/main.go
	cp -rf $(srcdir)/src $(builddir) || exit 0
	$(MKDIR_P) $(builddir)/src/msg_size_go/vendor
	rm -rf $(builddir)/src/msg_size_go/vendor/cloudi
	cp -rf $(top_srcdir)/api/go/cloudi $(builddir)/src/msg_size_go/vendor/
	rm -rf $(builddir)/src/msg_size_go/vendor/erlang
	cp -rf $(top_srcdir)/api/go/erlang $(builddir)/src/msg_size_go/vendor/
	$(MKDIR_P) $(builddir)/bin
	GOPATH=$(abs_builddir) GOBIN=$$GOPATH/bin $(GO) install -x msg_size_go
	cp $(builddir)/bin/msg_size_go .

clean-local:
	rm -rf $(builddir)/src/msg_size_go/vendor/
	rm -rf $(builddir)/pkg
	rm -rf $(builddir)/bin
	rm -f msg_size_go

install-exec-hook: msg_size_go
	$(MKDIR_P) $(instdir)
	$(INSTALL_SCRIPT) msg_size_go $(instdir)

