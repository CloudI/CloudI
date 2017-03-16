#-*-Mode:make;coding:utf-8;tab-width:4;c-basic-offset:4-*-
# ex: set ft=make fenc=utf-8 sts=4 ts=4 sw=4 noet nomod:

instdir = "$(cloudi_prefix)/api/haskell"
directinstdir = "$(DESTDIR)$(instdir)"
haskelldir = "$(instdir)"
haskell_DATA = dist/build/libHScloudi-$(VERSION).a
CLEANFILES = dist cabal.sandbox.config $(CABAL_SANDBOX)

all-local:
	$(CABAL) sandbox init --sandbox $(CABAL_SANDBOX)
	$(CABAL) sandbox add-source $(srcdir)/external/binary-0.9.0.0
	$(CABAL) sandbox add-source $(srcdir)/external/bytestring-0.10.8.1
	$(CABAL) sandbox add-source $(srcdir)/external/network-2.6.3.1
	$(CABAL) sandbox add-source $(srcdir)/external/unix-2.7.2.1
	$(CABAL) sandbox add-source $(srcdir)/external/zlib-0.6.1.2
	$(CABAL) install --only-dependencies
	$(CABAL) configure
	$(CABAL) build

install-exec-hook: $(haskell_DATA)
	$(MKDIR_P) $(directinstdir)
	$(INSTALL_DATA) dist/build/libHScloudi-$(VERSION).a $(directinstdir)

