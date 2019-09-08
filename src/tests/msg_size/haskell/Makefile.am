#-*-Mode:make;coding:utf-8;tab-width:4;c-basic-offset:4-*-
# ex: set ft=make fenc=utf-8 sts=4 ts=4 sw=4 noet nomod:

instdir = "$(DESTDIR)$(cloudi_prefix)/tests/msg_size"
CLEANFILES = cabal.sandbox.config msg_size_haskell

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

all-local: cabal.sandbox.config msg_size_haskell

cabal.sandbox.config:
	$(CABAL) $(CABAL_CMD_SANDBOX) init --sandbox $(CABAL_SANDBOX)

msg_size_haskell: Main.hs
	$(CABAL) $(CABAL_CMD_INSTALL) --only-dependencies --force-reinstalls \
                                      --with-compiler=$(GHC)
	$(CABAL) $(CABAL_CMD_CONFIGURE) --with-compiler=$(GHC)
	$(CABAL) $(CABAL_CMD_BUILD)
	cp $(builddir)/dist/build/msg_size_haskell/msg_size_haskell .

clean-local:
	rm -rf $(builddir)/dist

install-exec-hook: msg_size_haskell
	$(MKDIR_P) $(instdir)
	$(INSTALL_SCRIPT) msg_size_haskell $(instdir)

