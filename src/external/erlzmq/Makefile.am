ACLOCAL_AMFLAGS = -I m4

TARBALL=zmq-@PACKAGE_VERSION@.tgz
TARBALL_EXCLUDE=/tmp/exclude.xxx

SUBDIRS = c_src src

docs:
	@${MAKE} --directory=src $@

gitdocs: docs
	./bin/update_docs

clean-bootstrap: distclean
	rm -fr aclocal.m4 autom4te.cache build-aux config.h.in configure \
		m4/l* m4/version Makefile.in priv doc zmq.tgz stamp-h1
dist:
	@echo Creating $(TARBALL)
	@DIR=$${PWD##*/} && pushd .. > /dev/null && \
	echo -e "*.o\n*.d\n.git\n*.tgz\n.*.*\n~\n.~*#\ntags\n" \
	     -e "aclocal.m4\nautom4te.cache\nbuild-aux\n" \
		 -e "config.*\nconfigure\nebin/*.beam\nm4/l*\nMakefile\n" \
		 -e "Makefile.in\nm4/version" > $(TARBALL_EXCLUDE) && \
	for f in priv; do \
        find $$DIR/$$f -type f -print >> $(TARBALL_EXCLUDE) ; \
	done && \
	tar zcf $(TARBALL) $$DIR --exclude-from $(TARBALL_EXCLUDE) && \
	mv $(TARBALL) $$DIR && \
	popd > /dev/null && \
	rm -f $(TARBALL_EXCLUDE) && \
	ls -l $(TARBALL)

