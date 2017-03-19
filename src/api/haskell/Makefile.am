#-*-Mode:make;coding:utf-8;tab-width:4;c-basic-offset:4-*-
# ex: set ft=make fenc=utf-8 sts=4 ts=4 sw=4 noet nomod:

instdir = "$(DESTDIR)$(cloudi_prefix)/api/haskell"

all-local: cabal.sandbox.config

cabal.sandbox.config: src/Foreign/CloudI.hs \
                      src/Foreign/CloudI/Instance.hs \
                      src/Foreign/Erlang.hs \
                      src/Foreign/Erlang/Function.hs \
                      src/Foreign/Erlang/Pid.hs \
                      src/Foreign/Erlang/Port.hs \
                      src/Foreign/Erlang/Reference.hs
	$(CABAL) sandbox init --sandbox $(CABAL_SANDBOX)
	$(CABAL) sandbox add-source $(srcdir)/external/binary-0.9.0.0
	$(CABAL) sandbox add-source $(srcdir)/external/bytestring-0.10.8.1
	$(CABAL) sandbox add-source $(srcdir)/external/network-2.6.3.1
	$(CABAL) sandbox add-source $(srcdir)/external/unix-2.7.2.1
	$(CABAL) sandbox add-source $(srcdir)/external/zlib-0.6.1.2
	$(CABAL) install --only-dependencies --force-reinstalls \
                     --with-compiler=$(GHC)
	$(CABAL) configure --with-compiler=$(GHC)
	$(CABAL) build
	$(CABAL) install --with-compiler=$(GHC)

clean-local:
	rm -rf $(builddir)/dist $(CABAL_SANDBOX)
	rm -f cabal.sandbox.config
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

install-exec-hook:
	$(MKDIR_P) $(instdir)
	$(INSTALL_DATA) cloudi.cabal $(instdir)
	$(MKDIR_P) $(instdir)/src
	$(MKDIR_P) $(instdir)/src/Foreign
	$(INSTALL_DATA) src/Foreign/CloudI.hs \
                    $(instdir)/src/Foreign
	$(INSTALL_DATA) src/Foreign/Erlang.hs \
                    $(instdir)/src/Foreign
	$(MKDIR_P) $(instdir)/src/Foreign/CloudI
	$(INSTALL_DATA) src/Foreign/CloudI/Instance.hs \
                    $(instdir)/src/Foreign/CloudI
	$(MKDIR_P) $(instdir)/src/Foreign/Erlang
	$(INSTALL_DATA) src/Foreign/Erlang/Function.hs \
                    $(instdir)/src/Foreign/Erlang
	$(INSTALL_DATA) src/Foreign/Erlang/Pid.hs \
                    $(instdir)/src/Foreign/Erlang
	$(INSTALL_DATA) src/Foreign/Erlang/Port.hs \
                    $(instdir)/src/Foreign/Erlang
	$(INSTALL_DATA) src/Foreign/Erlang/Reference.hs \
                    $(instdir)/src/Foreign/Erlang

