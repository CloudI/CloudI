#-*-Mode:make;coding:utf-8;tab-width:4;c-basic-offset:4-*-
# ex: set ft=make fenc=utf-8 sts=4 ts=4 sw=4 noet nomod:

instdir = "$(DESTDIR)$(cloudi_prefix)/tests/msg_size/priv"

all-local: msg_size_haskell

cabal.sandbox.config:
	$(CABAL) sandbox init --sandbox $(CABAL_SANDBOX)

msg_size_haskell: cabal.sandbox.config \
                  Main.hs \
                  $(CABAL_SANDBOX)
	$(CABAL) install --only-dependencies --force-reinstalls \
                     --with-compiler=$(GHC)
	$(CABAL) configure --with-compiler=$(GHC)
	$(CABAL) build
	cp $(builddir)/dist/build/msg_size_haskell/msg_size_haskell .

clean-local:
	rm -rf $(builddir)/dist
	rm -f cabal.sandbox.config msg_size_haskell

install-exec-hook: msg_size_haskell
	$(MKDIR_P) $(instdir)
	$(INSTALL_SCRIPT) msg_size_haskell $(instdir)

