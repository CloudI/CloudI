#-*-Mode:make;coding:utf-8;tab-width:4;c-basic-offset:4-*-
# ex: set ft=make fenc=utf-8 sts=4 ts=4 sw=4 noet nomod:

instdir = "$(DESTDIR)$(cloudi_prefix)/tests/messaging"
CLEANFILES = cabal.sandbox.config messaging_haskell

if CABAL_INSTALL_V2
CABAL_CMD_SANDBOX=v1-sandbox
CABAL_CMD_INSTALL=v1-install
CABAL_CMD_CONFIGURE=v1-configure
CABAL_CMD_BUILD=v1-build
else
CABAL_CMD_SANDBOX=sandbox
CABAL_CMD_INSTALL=install
CABAL_CMD_CONFIGURE=configure
CABAL_CMD_BUILD=build
endif

.NOTPARALLEL:

all-local: cabal.sandbox.config messaging_haskell

cabal.sandbox.config:
	$(CABAL) $(CABAL_CMD_SANDBOX) init --sandbox $(CABAL_SANDBOX)

messaging_haskell: Main.hs
	$(CABAL) $(CABAL_CMD_INSTALL) --only-dependencies --force-reinstalls \
                                      --with-compiler=$(GHC)
	$(CABAL) $(CABAL_CMD_CONFIGURE) --with-compiler=$(GHC)
	$(CABAL) $(CABAL_CMD_BUILD)
	cp $(builddir)/dist/build/messaging_haskell/messaging_haskell .

clean-local:
	rm -rf $(builddir)/dist

install-exec-hook: messaging_haskell
	$(MKDIR_P) $(instdir)
	$(INSTALL_SCRIPT) messaging_haskell $(instdir)

