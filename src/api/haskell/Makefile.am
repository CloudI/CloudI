#-*-Mode:make;coding:utf-8;tab-width:4;c-basic-offset:4-*-
# ex: set ft=make fenc=utf-8 sts=4 ts=4 sw=4 noet nomod:

instdir = "$(DESTDIR)$(cloudi_prefix)/api/haskell"

HASKELL_NETWORK_VERSION=3.1.2.7
HASKELL_ZLIB_VERSION=0.6.3.0
if CABAL_INSTALL_V2_USE
HASKELL_HSC2HS_VERSION=0.68.8
CLEANFILES = cabal.project.local

all-local: cabal.project.local

cabal.project.local: src/Foreign/CloudI.hs \
                     src/Foreign/CloudI/Instance.hs \
                     src/Foreign/Erlang.hs \
                     src/Foreign/Erlang/Function.hs \
                     src/Foreign/Erlang/Pid.hs \
                     src/Foreign/Erlang/Port.hs \
                     src/Foreign/Erlang/Reference.hs
	$(MKDIR_P) $(CABAL_SANDBOX)
	$(CABAL) --store-dir=$(CABAL_SANDBOX) v2-configure \
                 --with-compiler=$(GHC) --enable-relocatable
	echo "packages: "$(srcdir)/external/hsc2hs-$(HASKELL_HSC2HS_VERSION)" "$(srcdir)/external/network-$(HASKELL_NETWORK_VERSION)" "$(srcdir)/external/zlib-$(HASKELL_ZLIB_VERSION) >> $(builddir)/cabal.project.local
	$(CABAL) v2-build

clean-local:
	rm -rf $(builddir)/dist-newstyle $(CABAL_SANDBOX)
	rm -rf $(srcdir)/external/network-$(HASKELL_NETWORK_VERSION)/dist
	rm -rf $(srcdir)/external/zlib-$(HASKELL_ZLIB_VERSION)/dist
	rm -f $(srcdir)/external/network-$(HASKELL_NETWORK_VERSION)/config.log
	rm -f $(srcdir)/external/network-$(HASKELL_NETWORK_VERSION)/config.status
	rm -f $(srcdir)/external/network-$(HASKELL_NETWORK_VERSION)/network.buildinfo

else
HASKELL_BINARY_VERSION=0.8.7.0
HASKELL_BYTESTRING_VERSION=0.10.10.0
HASKELL_UNIX_VERSION=2.7.2.2
CLEANFILES = cabal.sandbox.config
if CABAL_INSTALL_V2_AVOID
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

all-local: cabal.sandbox.config

cabal.sandbox.config: src/Foreign/CloudI.hs \
                      src/Foreign/CloudI/Instance.hs \
                      src/Foreign/Erlang.hs \
                      src/Foreign/Erlang/Function.hs \
                      src/Foreign/Erlang/Pid.hs \
                      src/Foreign/Erlang/Port.hs \
                      src/Foreign/Erlang/Reference.hs
	$(CABAL) $(CABAL_CMD_SANDBOX) init --sandbox $(CABAL_SANDBOX)
	$(CABAL) $(CABAL_CMD_SANDBOX) add-source \
             $(srcdir)/external/binary-$(HASKELL_BINARY_VERSION)
	$(CABAL) $(CABAL_CMD_SANDBOX) add-source \
             $(srcdir)/external/bytestring-$(HASKELL_BYTESTRING_VERSION)
	$(CABAL) $(CABAL_CMD_SANDBOX) add-source \
             $(srcdir)/external/network-$(HASKELL_NETWORK_VERSION)
	$(CABAL) $(CABAL_CMD_SANDBOX) add-source \
             $(srcdir)/external/unix-$(HASKELL_UNIX_VERSION)
	$(CABAL) $(CABAL_CMD_SANDBOX) add-source \
             $(srcdir)/external/zlib-$(HASKELL_ZLIB_VERSION)
	$(CABAL) $(CABAL_CMD_INSTALL) --only-dependencies --force-reinstalls \
                                  --with-compiler=$(GHC)
	$(CABAL) $(CABAL_CMD_CONFIGURE) --with-compiler=$(GHC)
	$(CABAL) $(CABAL_CMD_BUILD)
	$(CABAL) $(CABAL_CMD_INSTALL) --with-compiler=$(GHC)

clean-local:
	rm -rf $(builddir)/dist $(CABAL_SANDBOX)
	rm -rf $(srcdir)/external/binary-$(HASKELL_BINARY_VERSION)/dist
	rm -rf $(srcdir)/external/bytestring-$(HASKELL_BYTESTRING_VERSION)/dist
	rm -rf $(srcdir)/external/network-$(HASKELL_NETWORK_VERSION)/dist
	rm -rf $(srcdir)/external/unix-$(HASKELL_UNIX_VERSION)/dist
	rm -rf $(srcdir)/external/zlib-$(HASKELL_ZLIB_VERSION)/dist
	rm -f $(srcdir)/external/network-$(HASKELL_NETWORK_VERSION)/config.log
	rm -f $(srcdir)/external/network-$(HASKELL_NETWORK_VERSION)/config.status
	rm -f $(srcdir)/external/network-$(HASKELL_NETWORK_VERSION)/network.buildinfo
	rm -f $(srcdir)/external/unix-$(HASKELL_UNIX_VERSION)/config.log
	rm -f $(srcdir)/external/unix-$(HASKELL_UNIX_VERSION)/config.status
	rm -f $(srcdir)/external/unix-$(HASKELL_UNIX_VERSION)/include/HsUnixConfig.h
	rm -f $(srcdir)/external/unix-$(HASKELL_UNIX_VERSION)/unix.buildinfo

endif

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

