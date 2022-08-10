#-*-Mode:make;coding:utf-8;tab-width:4;c-basic-offset:4-*-
# ex: set ft=make fenc=utf-8 sts=4 ts=4 sw=4 noet nomod:

instdir = "$(DESTDIR)$(cloudi_prefix)/tests/null"
CLEANFILES = null_haskell

all-local: null_haskell

null_haskell: Main.hs Setup.hs
	$(MKDIR_P) dist/setup-bin
	$(GHC) --make -outputdir dist/setup-bin -o dist/setup-bin/Setup $(srcdir)/Setup.hs
	dist/setup-bin/Setup configure --builddir=$(builddir)/dist --package-db=$(CABAL_SANDBOX_PKG_DB) --enable-deterministic --disable-shared --enable-static
	dist/setup-bin/Setup build
	mv -f dist/build/$@/$@ $(builddir)

clean-local:
	rm -rf $(builddir)/dist

install-exec-hook: null_haskell
	$(MKDIR_P) $(instdir)
	$(INSTALL_SCRIPT) null_haskell $(instdir)

