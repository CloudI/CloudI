#-*-Mode:make;coding:utf-8;tab-width:4;c-basic-offset:4-*-
# ex: set ft=make fenc=utf-8 sts=4 ts=4 sw=4 noet nomod:

instdir = "$(DESTDIR)$(cloudi_prefix)/tests/messaging/priv"

all-local: messaging_haskell

cabal.sandbox.config:
	$(CABAL) sandbox init --sandbox $(CABAL_SANDBOX)

messaging_haskell: cabal.sandbox.config \
                   Main.hs \
                   $(CABAL_SANDBOX)
	$(CABAL) install --only-dependencies --force-reinstalls \
                     --with-compiler=$(GHC)
	$(CABAL) configure --with-compiler=$(GHC)
	$(CABAL) build
	cp $(builddir)/dist/build/messaging_haskell/messaging_haskell .

clean-local:
	rm -rf $(builddir)/dist
	rm -f cabal.sandbox.config messaging_haskell

install-exec-hook: messaging_haskell
	$(MKDIR_P) $(instdir)
	$(INSTALL_SCRIPT) messaging_haskell $(instdir)

