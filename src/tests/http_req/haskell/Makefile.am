#-*-Mode:make;coding:utf-8;tab-width:4;c-basic-offset:4-*-
# ex: set ft=make fenc=utf-8 sts=4 ts=4 sw=4 noet nomod:

instdir = "$(DESTDIR)$(cloudi_prefix)/tests/http_req/priv"

all-local: http_req_haskell

cabal.sandbox.config:
	$(CABAL) sandbox init --sandbox $(CABAL_SANDBOX)

http_req_haskell: cabal.sandbox.config \
                  Main.hs \
                  $(CABAL_SANDBOX)
	$(CABAL) install --only-dependencies --force-reinstalls \
                     --with-compiler=$(GHC)
	$(CABAL) configure --with-compiler=$(GHC)
	$(CABAL) build
	cp $(builddir)/dist/build/http_req_haskell/http_req_haskell .

clean-local:
	rm -rf $(builddir)/dist
	rm -f cabal.sandbox.config http_req_haskell

install-exec-hook: http_req_haskell
	$(MKDIR_P) $(instdir)
	$(INSTALL_SCRIPT) http_req_haskell $(instdir)

