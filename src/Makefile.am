ACLOCAL_AMFLAGS = -I m4
SUBDIRS = api lib tests

all-local: rebar 
	./rebar compile

clean-local: rebar
	./rebar clean
	rm -f api/python/cloudi.pyc api/python/erlang.pyc
	rm -rf rel/cloudi

clean-configure: config.log
	rm -f aclocal.m4 config.h config.h.in config.h.in~ config.log \
          config.status stamp-h1 \
          config/config.guess config/config.sub config/depcomp \
          config/install-sh config/ltmain.sh config/missing \
          rebar Makefile Makefile.in configure \
          m4/libtool.m4 m4/ltoptions.m4 m4/ltsugar.m4 \
          m4/ltversion.m4 m4/lt~obsolete.m4 libtool \
          external/rebar/ebin/*.beam
	rm -rf autom4te.cache
	rm -rf api/c/.deps/ \
           lib/cloudi/cxx_src/.deps/ \
           tests/hexpi/cxx_src/.deps/
	find api -name "Makefile.in" -o -name "Makefile" | xargs rm -f
	find lib -name "Makefile.in" -o -name "Makefile" | xargs rm -f
	find tests -name "Makefile.in" -o -name "Makefile" | xargs rm -f

dialyze: rebar
	./rebar dialyze

run: all
	$(ERLANG_ROOT_DIR)/bin/erl -config config/sys \
      -pz external/epgsql/ebin \
      -pz external/emysql/ebin \
      -pz external/etokyotyrant/ebin \
      -pz external/ememcached/ebin \
      -pz external/ecouchdb/ebin \
      -pz external/misultin/ebin \
      -pz tests/hexpi/ebin \
      `find lib/ -name "ebin" -o -name "priv" | sed 's/^/-pz /'`

install-exec-hook: tests-install \
                   tests-hexpi-install \
                   tests-http-install \
                   api-install \
                   api-c-install \
                   api-java-install \
                   api-python-install
	rm -rf $(DESTDIR)$(bindir)
	rm -rf $(DESTDIR)$(includedir)
	rm -rf $(DESTDIR)$(libdir)
	rm -rf $(DESTDIR)$(libexecdir)
	rm -rf $(DESTDIR)$(datarootdir)
	test ! -d $(top_builddir)/rel/cloudi/ && ./rebar generate || exit 0
	cp -rf $(top_builddir)/rel/cloudi/bin $(DESTDIR)$(bindir)
	cp -rf $(top_builddir)/rel/cloudi/lib $(DESTDIR)$(libdir)
	cp -rf $(top_builddir)/rel/cloudi/etc $(DESTDIR)$(sysconfdir)
	cp -rf $(top_builddir)/rel/cloudi/erts-* $(DESTDIR)$(prefix)
	cp -rf $(top_builddir)/rel/cloudi/releases $(DESTDIR)$(prefix)
	mkdir $(DESTDIR)$(prefix)/logs
	cp -f $(top_builddir)/cloudi.conf $(DESTDIR)$(prefix)

tests-install:
	mkdir $(DESTDIR)${prefix}/tests

tests-hexpi-install: $(top_builddir)/tests/hexpi/ebin/cloudi_job_hexpi.beam
	mkdir $(DESTDIR)${prefix}/tests/hexpi
	mkdir $(DESTDIR)${prefix}/tests/hexpi/ebin
	cp -f $(top_builddir)/tests/hexpi/ebin/cloudi_job_hexpi.beam \
          $(DESTDIR)${prefix}/tests/hexpi/ebin/
	mkdir $(DESTDIR)${prefix}/tests/hexpi/priv
	mv -f $(DESTDIR)$(bindir)/hexpi \
          $(DESTDIR)${prefix}/tests/hexpi/priv/

tests-http-install: $(top_builddir)/tests/http/service/jar/cloudi.jar \
                    $(top_builddir)/tests/http/service/jar/service.jar
	mkdir $(DESTDIR)${prefix}/tests/http
	cp -f $(top_builddir)/tests/http/run.sh \
          $(DESTDIR)${prefix}/tests/http/
	mkdir $(DESTDIR)${prefix}/tests/http/service
	mkdir $(DESTDIR)${prefix}/tests/http/service/jar
	cp -f $(top_builddir)/tests/http/service/jar/cloudi.jar \
          $(DESTDIR)${prefix}/tests/http/service/jar/
	cp -f $(top_builddir)/tests/http/service/jar/service.jar \
          $(DESTDIR)${prefix}/tests/http/service/jar/
	cp -f $(top_builddir)/tests/http/service/service.py \
          $(DESTDIR)${prefix}/tests/http/service/
	mkdir $(DESTDIR)${prefix}/tests/http/input
	cp -f $(top_builddir)/tests/http/input/* \
          $(DESTDIR)${prefix}/tests/http/input/
	mkdir $(DESTDIR)${prefix}/tests/http/input.zip
	cp -f $(top_builddir)/tests/http/input.zip/* \
          $(DESTDIR)${prefix}/tests/http/input.zip/
	mkdir $(DESTDIR)${prefix}/tests/http/output
	cp -f $(top_builddir)/tests/http/output/* \
          $(DESTDIR)${prefix}/tests/http/output/
	mkdir $(DESTDIR)${prefix}/tests/http/tmp

api-install:
	mkdir $(DESTDIR)${prefix}/api

api-c-install:
	mkdir $(DESTDIR)${prefix}/api/c
	mkdir $(DESTDIR)${prefix}/api/c/include
	mkdir $(DESTDIR)${prefix}/api/c/lib
	mv -f $(DESTDIR)$(includedir)/cloudi.h \
          $(DESTDIR)${prefix}/api/c/include/
	mv -f $(DESTDIR)$(libdir)/libcloudi.* \
          $(DESTDIR)${prefix}/api/c/lib/

api-java-install:
	mkdir $(DESTDIR)${prefix}/api/java
	mv -f $(DESTDIR)$(libexecdir)/cloudi.jar \
          $(DESTDIR)${prefix}/api/java

api-python-install:
	mkdir $(DESTDIR)${prefix}/api/python
	cp -f $(top_builddir)/api/python/erlang.py \
          $(DESTDIR)${prefix}/api/python/
	cp -f $(top_builddir)/api/python/cloudi.py \
          $(DESTDIR)${prefix}/api/python/
