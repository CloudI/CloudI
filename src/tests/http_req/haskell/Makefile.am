#-*-Mode:make;coding:utf-8;tab-width:4;c-basic-offset:4-*-
# ex: set ft=make fenc=utf-8 sts=4 ts=4 sw=4 noet nomod:

instdir = "$(DESTDIR)$(cloudi_prefix)/tests/http_req"
CLEANFILES = cabal.sandbox.config http_req_haskell

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

all-local: cabal.sandbox.config http_req_haskell

cabal.sandbox.config:
	$(CABAL) $(CABAL_CMD_SANDBOX) init --sandbox $(CABAL_SANDBOX)

http_req_haskell: Main.hs
	$(CABAL) $(CABAL_CMD_INSTALL) --only-dependencies --force-reinstalls \
                                      --with-compiler=$(GHC)
	$(CABAL) $(CABAL_CMD_CONFIGURE) --with-compiler=$(GHC)
	$(CABAL) $(CABAL_CMD_BUILD)
	cp $(builddir)/dist/build/http_req_haskell/http_req_haskell .

clean-local:
	rm -rf $(builddir)/dist

install-exec-hook: http_req_haskell
	$(MKDIR_P) $(instdir)
	$(INSTALL_SCRIPT) http_req_haskell $(instdir)

