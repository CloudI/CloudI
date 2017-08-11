#-*-Mode:make;coding:utf-8;tab-width:4;c-basic-offset:4-*-
# ex: set ft=make fenc=utf-8 sts=4 ts=4 sw=4 noet nomod:

instdir = "$(DESTDIR)$(cloudi_prefix)/tests/null"
CLEANFILES = cabal.sandbox.config null_haskell

.NOTPARALLEL:

all-local: cabal.sandbox.config null_haskell

cabal.sandbox.config:
	$(CABAL) sandbox init --sandbox $(CABAL_SANDBOX)

null_haskell: Main.hs \
              $(CABAL_SANDBOX)
	$(CABAL) install --only-dependencies --force-reinstalls \
                     --with-compiler=$(GHC)
	$(CABAL) configure --with-compiler=$(GHC)
	$(CABAL) build
	cp $(builddir)/dist/build/null_haskell/null_haskell .

clean-local:
	rm -rf $(builddir)/dist

install-exec-hook: null_haskell
	$(MKDIR_P) $(instdir)
	$(INSTALL_SCRIPT) null_haskell $(instdir)

