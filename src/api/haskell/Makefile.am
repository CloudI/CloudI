#-*-Mode:make;coding:utf-8;tab-width:4;c-basic-offset:4-*-
# ex: set ft=make fenc=utf-8 sts=4 ts=4 sw=4 noet nomod:

instdir = "$(DESTDIR)$(cloudi_prefix)/api/haskell"
CLEANFILES = built

HASKELL_CLOUDI_CONFIG=$(abs_builddir)/dist/package.conf.inplace/cloudi-$(VERSION).conf
HASKELL_NETWORK_VERSION=3.1.2.7
HASKELL_NETWORK_SRCDIR=$(srcdir)/external/network-$(HASKELL_NETWORK_VERSION)
HASKELL_NETWORK_BUILDDIR=$(abs_builddir)/external/network-$(HASKELL_NETWORK_VERSION)/dist
HASKELL_NETWORK_CONFIG=$(HASKELL_NETWORK_BUILDDIR)/package.conf.inplace/network-$(HASKELL_NETWORK_VERSION).conf
HASKELL_ZLIB_VERSION=0.6.3.0
HASKELL_ZLIB_SRCDIR=$(srcdir)/external/zlib-$(HASKELL_ZLIB_VERSION)
HASKELL_ZLIB_BUILDDIR=$(abs_builddir)/external/zlib-$(HASKELL_ZLIB_VERSION)/dist
HASKELL_ZLIB_CONFIG=$(HASKELL_ZLIB_BUILDDIR)/package.conf.inplace/zlib-$(HASKELL_ZLIB_VERSION).conf

all-local: built

if GHC_PACKAGES_V9

built: $(CABAL_SANDBOX_PKG_DB) \
       Setup.hs \
       src/Foreign/CloudI.hs \
       src/Foreign/CloudI/Instance.hs \
       src/Foreign/Erlang.hs \
       src/Foreign/Erlang/Function.hs \
       src/Foreign/Erlang/Pid.hs \
       src/Foreign/Erlang/Port.hs \
       src/Foreign/Erlang/Reference.hs \
       $(HASKELL_NETWORK_CONFIG) \
       $(HASKELL_ZLIB_CONFIG)
	$(MKDIR_P) dist/setup-bin
	$(GHC) --make -outputdir dist/setup-bin -o dist/setup-bin/Setup $(srcdir)/Setup.hs
	dist/setup-bin/Setup configure --builddir=$(builddir)/dist --package-db=$(CABAL_SANDBOX_PKG_DB) --enable-deterministic --disable-shared --enable-static
	dist/setup-bin/Setup build
	$(GHC_PKG) -v0 --global-package-db=$(CABAL_SANDBOX_PKG_DB) --force register $(HASKELL_CLOUDI_CONFIG)
	touch built

clean-local:
	rm -rf $(CABAL_SANDBOX) dist \
           $(HASKELL_NETWORK_BUILDDIR) \
           $(HASKELL_ZLIB_BUILDDIR)
	rm -f $(HASKELL_NETWORK_SRCDIR)/config.log \
          $(HASKELL_NETWORK_SRCDIR)/config.status \
          $(HASKELL_NETWORK_SRCDIR)/network.buildinfo

else

HASKELL_BINARY_VERSION=0.8.7.0
HASKELL_BINARY_SRCDIR=$(srcdir)/external/binary-$(HASKELL_BINARY_VERSION)
HASKELL_BINARY_BUILDDIR=$(abs_builddir)/external/binary-$(HASKELL_BINARY_VERSION)/dist
HASKELL_BINARY_CONFIG=$(HASKELL_BINARY_BUILDDIR)/package.conf.inplace/binary-$(HASKELL_BINARY_VERSION).conf
HASKELL_BYTESTRING_VERSION=0.10.10.0
HASKELL_BYTESTRING_SRCDIR=$(srcdir)/external/bytestring-$(HASKELL_BYTESTRING_VERSION)
HASKELL_BYTESTRING_BUILDDIR=$(abs_builddir)/external/bytestring-$(HASKELL_BYTESTRING_VERSION)/dist
HASKELL_BYTESTRING_CONFIG=$(HASKELL_BYTESTRING_BUILDDIR)/package.conf.inplace/bytestring-$(HASKELL_BYTESTRING_VERSION).conf
HASKELL_UNIX_VERSION=2.7.2.2
HASKELL_UNIX_SRCDIR=$(srcdir)/external/unix-$(HASKELL_UNIX_VERSION)
HASKELL_UNIX_BUILDDIR=$(abs_builddir)/external/unix-$(HASKELL_UNIX_VERSION)/dist
HASKELL_UNIX_CONFIG=$(HASKELL_UNIX_BUILDDIR)/package.conf.inplace/unix-$(HASKELL_UNIX_VERSION).conf

built: $(CABAL_SANDBOX_PKG_DB) \
       Setup.hs \
       src/Foreign/CloudI.hs \
       src/Foreign/CloudI/Instance.hs \
       src/Foreign/Erlang.hs \
       src/Foreign/Erlang/Function.hs \
       src/Foreign/Erlang/Pid.hs \
       src/Foreign/Erlang/Port.hs \
       src/Foreign/Erlang/Reference.hs \
       $(HASKELL_BYTESTRING_CONFIG) \
       $(HASKELL_BINARY_CONFIG) \
       $(HASKELL_UNIX_CONFIG) \
       $(HASKELL_NETWORK_CONFIG) \
       $(HASKELL_ZLIB_CONFIG)
	$(MKDIR_P) dist/setup-bin
	$(GHC) --make -outputdir dist/setup-bin -o dist/setup-bin/Setup $(srcdir)/Setup.hs
	dist/setup-bin/Setup configure --builddir=$(builddir)/dist --package-db=$(CABAL_SANDBOX_PKG_DB) --enable-deterministic --disable-shared --enable-static
	dist/setup-bin/Setup build
	$(GHC_PKG) -v0 --global-package-db=$(CABAL_SANDBOX_PKG_DB) --force register $(HASKELL_CLOUDI_CONFIG)
	touch built

clean-local:
	rm -rf $(CABAL_SANDBOX) dist \
           $(HASKELL_BINARY_BUILDDIR) \
           $(HASKELL_BYTESTRING_BUILDDIR) \
           $(HASKELL_NETWORK_BUILDDIR) \
           $(HASKELL_UNIX_BUILDDIR) \
           $(HASKELL_ZLIB_BUILDDIR)
	rm -f $(HASKELL_NETWORK_SRCDIR)/config.log \
          $(HASKELL_NETWORK_SRCDIR)/config.status \
          $(HASKELL_NETWORK_SRCDIR)/network.buildinfo \
          $(HASKELL_UNIX_SRCDIR)/config.log \
          $(HASKELL_UNIX_SRCDIR)/config.status \
          $(HASKELL_UNIX_SRCDIR)/include/HsUnixConfig.h \
          $(HASKELL_UNIX_SRCDIR)/unix.buildinfo

$(HASKELL_BYTESTRING_CONFIG):
	$(MKDIR_P) $(HASKELL_BYTESTRING_BUILDDIR)/setup-bin && \
        cd $(HASKELL_BYTESTRING_SRCDIR) && \
        $(GHC) --make -outputdir $(HASKELL_BYTESTRING_BUILDDIR)/setup-bin -o $(HASKELL_BYTESTRING_BUILDDIR)/setup-bin/Setup ./Setup.hs && \
        $(HASKELL_BYTESTRING_BUILDDIR)/setup-bin/Setup configure --builddir=$(HASKELL_BYTESTRING_BUILDDIR) --package-db=$(CABAL_SANDBOX_PKG_DB) --enable-deterministic --disable-shared --enable-static && \
        $(HASKELL_BYTESTRING_BUILDDIR)/setup-bin/Setup build && \
        $(GHC_PKG) -v0 --global-package-db=$(CABAL_SANDBOX_PKG_DB) --force register $(HASKELL_BYTESTRING_CONFIG)

$(HASKELL_BINARY_CONFIG):
	$(MKDIR_P) $(HASKELL_BINARY_BUILDDIR)/setup-bin && \
        cd $(HASKELL_BINARY_SRCDIR) && \
        $(GHC) --make -outputdir $(HASKELL_BINARY_BUILDDIR)/setup-bin -o $(HASKELL_BINARY_BUILDDIR)/setup-bin/Setup ./Setup.lhs && \
        $(HASKELL_BINARY_BUILDDIR)/setup-bin/Setup configure --builddir=$(HASKELL_BINARY_BUILDDIR) --package-db=$(CABAL_SANDBOX_PKG_DB) --enable-deterministic --disable-shared --enable-static && \
        $(HASKELL_BINARY_BUILDDIR)/setup-bin/Setup build && \
        $(GHC_PKG) -v0 --global-package-db=$(CABAL_SANDBOX_PKG_DB) --force register $(HASKELL_BINARY_CONFIG)

$(HASKELL_UNIX_CONFIG):
	$(MKDIR_P) $(HASKELL_UNIX_BUILDDIR)/setup-bin && \
        cd $(HASKELL_UNIX_SRCDIR) && \
        $(GHC) --make -outputdir $(HASKELL_UNIX_BUILDDIR)/setup-bin -o $(HASKELL_UNIX_BUILDDIR)/setup-bin/Setup ./Setup.hs && \
        $(HASKELL_UNIX_BUILDDIR)/setup-bin/Setup configure --builddir=$(HASKELL_UNIX_BUILDDIR) --package-db=$(CABAL_SANDBOX_PKG_DB) --enable-deterministic --disable-shared --enable-static && \
        $(HASKELL_UNIX_BUILDDIR)/setup-bin/Setup build && \
        $(GHC_PKG) -v0 --global-package-db=$(CABAL_SANDBOX_PKG_DB) --force register $(HASKELL_UNIX_CONFIG)

endif

$(CABAL_SANDBOX_PKG_DB):
	$(MKDIR_P) $(CABAL_SANDBOX)
	$(GHC_PKG) init $(CABAL_SANDBOX_PKG_DB)

$(HASKELL_NETWORK_CONFIG):
	$(MKDIR_P) $(HASKELL_NETWORK_BUILDDIR)/setup-bin && \
        cd $(HASKELL_NETWORK_SRCDIR) && \
        $(GHC) --make -outputdir $(HASKELL_NETWORK_BUILDDIR)/setup-bin -o $(HASKELL_NETWORK_BUILDDIR)/setup-bin/Setup ./Setup.hs && \
        $(HASKELL_NETWORK_BUILDDIR)/setup-bin/Setup configure --builddir=$(HASKELL_NETWORK_BUILDDIR) --package-db=$(CABAL_SANDBOX_PKG_DB) --enable-deterministic --disable-shared --enable-static && \
        $(HASKELL_NETWORK_BUILDDIR)/setup-bin/Setup build && \
        $(GHC_PKG) -v0 --global-package-db=$(CABAL_SANDBOX_PKG_DB) --force register $(HASKELL_NETWORK_CONFIG)

$(HASKELL_ZLIB_CONFIG):
	$(MKDIR_P) $(HASKELL_ZLIB_BUILDDIR)/setup-bin && \
        cd $(HASKELL_ZLIB_SRCDIR) && \
        $(GHC) --make -outputdir $(HASKELL_ZLIB_BUILDDIR)/setup-bin -o $(HASKELL_ZLIB_BUILDDIR)/setup-bin/Setup ./Setup.hs && \
        $(HASKELL_ZLIB_BUILDDIR)/setup-bin/Setup configure --builddir=$(HASKELL_ZLIB_BUILDDIR) --package-db=$(CABAL_SANDBOX_PKG_DB) --enable-deterministic --disable-shared --enable-static && \
        $(HASKELL_ZLIB_BUILDDIR)/setup-bin/Setup build && \
        $(GHC_PKG) -v0 --global-package-db=$(CABAL_SANDBOX_PKG_DB) --force register $(HASKELL_ZLIB_CONFIG)

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

