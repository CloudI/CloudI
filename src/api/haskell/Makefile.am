#-*-Mode:make;coding:utf-8;tab-width:4;c-basic-offset:4-*-
# ex: set ft=make fenc=utf-8 sts=4 ts=4 sw=4 noet nomod:

instdir = "$(DESTDIR)$(cloudi_prefix)/api/haskell"

all-local: libHScloudi-$(VERSION).a

libHScloudi-$(VERSION).a:
	$(CABAL) sandbox init --sandbox $(CABAL_SANDBOX)
	$(CABAL) sandbox add-source $(srcdir)/external/binary-0.9.0.0
	$(CABAL) sandbox add-source $(srcdir)/external/bytestring-0.10.8.1
	$(CABAL) sandbox add-source $(srcdir)/external/network-2.6.3.1
	$(CABAL) sandbox add-source $(srcdir)/external/unix-2.7.2.1
	$(CABAL) sandbox add-source $(srcdir)/external/zlib-0.6.1.2
	$(CABAL) install --only-dependencies
	$(CABAL) configure
	$(CABAL) build
	cp $(builddir)/dist/build/libHScloudi-$(VERSION).a .

clean-local:
	rm -rf $(builddir)/dist $(CABAL_SANDBOX)
	rm -f $(builddir)/cabal.sandbox.config libHScloudi-$(VERSION).a
	rm -rf $(srcdir)/external/binary-0.9.0.0/dist
	rm -rf $(srcdir)/external/bytestring-0.10.8.1/dist
	rm -rf $(srcdir)/external/network-2.6.3.1/dist
	rm -rf $(srcdir)/external/unix-2.7.2.1/dist
	rm -rf $(srcdir)/external/zlib-0.6.1.2/dist
	rm -f $(srcdir)/external/network-2.6.3.1/config.log
	rm -f $(srcdir)/external/network-2.6.3.1/config.status
	rm -f $(srcdir)/external/network-2.6.3.1/network.buildinfo
	rm -f $(srcdir)/external/unix-2.7.2.1/config.log
	rm -f $(srcdir)/external/unix-2.7.2.1/config.status
	rm -f $(srcdir)/external/unix-2.7.2.1/include/HsUnixConfig.h
	rm -f $(srcdir)/external/unix-2.7.2.1/unix.buildinfo

install-exec-hook: libHScloudi-$(VERSION).a
	$(MKDIR_P) $(instdir)
	$(INSTALL_DATA) libHScloudi-$(VERSION).a $(instdir)

