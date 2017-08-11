#-*-Mode:make;coding:utf-8;tab-width:4;c-basic-offset:4-*-
# ex: set ft=make fenc=utf-8 sts=4 ts=4 sw=4 noet nomod:

instdir = "$(DESTDIR)$(cloudi_prefix)/tests/msg_size"
CLEANFILES = cabal.sandbox.config msg_size_haskell

.NOTPARALLEL:

all-local: cabal.sandbox.config msg_size_haskell

cabal.sandbox.config:
	$(CABAL) sandbox init --sandbox $(CABAL_SANDBOX)

msg_size_haskell: Main.hs \
                  $(CABAL_SANDBOX)
	$(CABAL) install --only-dependencies --force-reinstalls \
                     --with-compiler=$(GHC)
	$(CABAL) configure --with-compiler=$(GHC)
	$(CABAL) build
	cp $(builddir)/dist/build/msg_size_haskell/msg_size_haskell .

clean-local:
	rm -rf $(builddir)/dist

install-exec-hook: msg_size_haskell
	$(MKDIR_P) $(instdir)
	$(INSTALL_SCRIPT) msg_size_haskell $(instdir)

