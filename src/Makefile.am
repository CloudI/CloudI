ACLOCAL_AMFLAGS = -I m4
SUBDIRS = api lib tests

all-local: rebar 
	$(ESCRIPT) rebar compile

clean-local: rebar
	$(ESCRIPT) rebar clean
	rm -f api/python/cloudi.pyc api/python/erlang.pyc

clean-configure: config.log
	(cd external/zeromq && make clean || exit 0)
	(cd external/erlzmq2 && ../../rebar clean || exit 0)
	rm -f aclocal.m4 config.h config.h.in config.h.in~ config.log \
          config.status stamp-h1 \
          config/config.guess config/config.sub config/depcomp \
          config/install-sh config/ltmain.sh config/missing \
          rebar Makefile Makefile.in configure \
          m4/libtool.m4 m4/ltoptions.m4 m4/ltsugar.m4 \
          m4/ltversion.m4 m4/lt~obsolete.m4 libtool \
          external/rebar/ebin/*.beam \
          lib/cloudi/src/cloudi.app.src reltool.config cloudi.conf
	rm -rf autom4te.cache
	rm -rf api/c/.deps/ \
           lib/cloudi/cxx_src/.deps/ \
           tests/hexpi/cxx_src/.deps/ \
           tests/http_req/c_src/.deps/
	find api -name "Makefile.in" -o -name "Makefile" | xargs rm -f
	find lib -name "Makefile.in" -o -name "Makefile" | xargs rm -f
	find tests -name "Makefile.in" -o -name "Makefile" | xargs rm -f

dialyze: rebar
	$(ESCRIPT) rebar dialyze

run: all
	$(ERLANG_ROOT_DIR)/bin/erl -config config/sys \
      -sname cloudi \
      -pz external/epgsql/ebin \
      -pz external/emysql/ebin \
      -pz external/etokyotyrant/ebin \
      -pz external/ememcached/ebin \
      -pz external/ecouchdb/ebin \
      -pz external/misultin/ebin \
      -pz tests/hexpi/ebin \
      `find lib/ -name "ebin" | sed 's/^/-pz /'`

distdir:
	@echo
	@echo "*************************************"
	@echo "* AUTOMAKE DISTRIBUTION IS NOT USED *"
	@echo "*************************************"
	@echo
	@exit 1

dist: distdir

uninstall:
	@echo
	@echo "**********************************"
	@echo "* AUTOMAKE UNINSTALL IS NOT USED *"
	@echo "**********************************"
	@echo
	@exit 1

install-exec-hook: tests-install \
                   tests-hexpi-install \
                   tests-http-install \
                   tests-http_req-install \
                   tests-zeromq-install \
                   api-install \
                   api-c-install \
                   api-java-install \
                   api-python-install \
                   api-ruby-install
	rm -f "$(DESTDIR)$(bindir)/cloudi_os_spawn_vsn_"*
	$(MKDIR_P) "$(DESTDIR)$(sysconfdir)"
	$(MKDIR_P) "$(DESTDIR)$(prefix)/releases"
	$(MKDIR_P) "$(DESTDIR)$(prefix)/logs"
	$(INSTALL_DATA) "$(top_builddir)/cloudi.conf" \
                    "$(DESTDIR)$(prefix)"
	$(ESCRIPT) rebar generate || exit 0
	test ! -d "$(DESTDIR)$(prefix)/erts-$(ERLANG_ERTS_VER)" && \
        mv -f "$(top_builddir)/rel/cloudi/erts-$(ERLANG_ERTS_VER)" \
              "$(DESTDIR)$(prefix)" || exit 0
	$(INSTALL_SCRIPT) "$(top_builddir)/rel/cloudi/bin/cloudi" \
                      "$(DESTDIR)$(bindir)/"
	$(INSTALL_DATA) "$(top_builddir)/rel/cloudi/etc/app.config" \
                    "$(DESTDIR)$(sysconfdir)"
	$(INSTALL_DATA) "$(top_builddir)/rel/cloudi/etc/vm.args" \
                    "$(DESTDIR)$(sysconfdir)"
	test ! -d "$(DESTDIR)$(prefix)/releases/1" && \
        mv -f "$(top_builddir)/rel/cloudi/releases/1" \
              "$(DESTDIR)$(prefix)/releases/" || exit 0
	$(INSTALL_DATA) "$(top_builddir)/rel/cloudi/releases/start_erl.data" \
                    "$(DESTDIR)$(prefix)/releases/"
	list=`ls "$(top_builddir)/rel/cloudi/lib/"`; for subdir in $$list; do \
        test ! -e "$(DESTDIR)$(prefix)/lib/$$subdir" && \
        mv -f "$(top_builddir)/rel/cloudi/lib/$$subdir" \
              "$(DESTDIR)$(prefix)/lib/" || exit 0; \
    done
	rm -rf $(top_builddir)/rel/cloudi/

tests-install:
	$(MKDIR_P) "$(DESTDIR)$(prefix)/tests"

tests-hexpi-install: $(top_builddir)/tests/hexpi/ebin/cloudi_job_hexpi.beam
	$(MKDIR_P) "$(DESTDIR)$(prefix)/tests/hexpi/ebin"
	$(MKDIR_P) "$(DESTDIR)$(prefix)/tests/hexpi/priv"
	$(INSTALL_DATA) "$(top_builddir)/tests/hexpi/ebin/cloudi_job_hexpi.beam" \
                    "$(DESTDIR)$(prefix)/tests/hexpi/ebin/"
	$(INSTALL_PROGRAM) "$(DESTDIR)$(bindir)/hexpi" \
                       "$(DESTDIR)$(prefix)/tests/hexpi/priv/"
	rm -f "$(DESTDIR)$(bindir)/hexpi"

tests-http-install: $(top_builddir)/tests/http/service/jar/cloudi.jar \
                    $(top_builddir)/tests/http/service/jar/service.jar
	$(MKDIR_P) "$(DESTDIR)$(prefix)/tests/http/service/jar"
	$(MKDIR_P) "$(DESTDIR)$(prefix)/tests/http/input"
	$(MKDIR_P) "$(DESTDIR)$(prefix)/tests/http/input.zip"
	$(MKDIR_P) "$(DESTDIR)$(prefix)/tests/http/output"
	$(MKDIR_P) "$(DESTDIR)$(prefix)/tests/http/tmp"
	$(INSTALL_SCRIPT) "$(top_builddir)/tests/http/run.sh" \
                      "$(DESTDIR)$(prefix)/tests/http/"
	$(INSTALL_DATA) "$(top_builddir)/tests/http/service/jar/cloudi.jar" \
                    "$(DESTDIR)$(prefix)/tests/http/service/jar/"
	$(INSTALL_DATA) "$(top_builddir)/tests/http/service/jar/service.jar" \
                    "$(DESTDIR)$(prefix)/tests/http/service/jar/"
	$(INSTALL_SCRIPT) "$(top_builddir)/tests/http/service/service.py" \
                      "$(DESTDIR)$(prefix)/tests/http/service/"
	$(INSTALL_DATA) "$(top_builddir)/tests/http/service/service.rb" \
                    "$(DESTDIR)$(prefix)/tests/http/service/"
	$(INSTALL_DATA) "$(top_builddir)/tests/http/input/text" \
                    "$(DESTDIR)$(prefix)/tests/http/input/"
	$(INSTALL_DATA) "$(top_builddir)/tests/http/input.zip/text" \
                    "$(DESTDIR)$(prefix)/tests/http/input.zip/"
	$(INSTALL_DATA) "$(top_builddir)/tests/http/output/text" \
                    "$(DESTDIR)$(prefix)/tests/http/output/"
	rm -f "$(DESTDIR)$(libexecdir)/service.jar"

tests-http_req-install:
	$(MKDIR_P) "$(DESTDIR)$(prefix)/tests/http_req/priv"
	$(MKDIR_P) "$(DESTDIR)$(prefix)/tests/http_req/public_html"
	$(INSTALL_PROGRAM) "$(DESTDIR)$(bindir)/http_req" \
                       "$(DESTDIR)$(prefix)/tests/http_req/priv/"
	$(INSTALL_DATA) "$(top_builddir)/tests/http_req/public_html/index.html" \
                    "$(DESTDIR)$(prefix)/tests/http_req/public_html/"
	$(INSTALL_DATA) \
        "$(top_builddir)/tests/http_req/public_html/800px-Juliadim2.png" \
        "$(DESTDIR)$(prefix)/tests/http_req/public_html/"
	rm -f "$(DESTDIR)$(bindir)/http_req"

tests-zeromq-install:
	$(MKDIR_P) "$(DESTDIR)$(prefix)/tests/zeromq"
	$(INSTALL_SCRIPT) "$(top_builddir)/tests/zeromq/run.py" \
                      "$(DESTDIR)$(prefix)/tests/zeromq/"

api-install:
	$(MKDIR_P) "$(DESTDIR)$(prefix)/api"

api-c-install:
	$(MKDIR_P) "$(DESTDIR)$(prefix)/api/c/include"
	$(MKDIR_P) "$(DESTDIR)$(prefix)/api/c/lib"
	$(INSTALL_DATA) "$(DESTDIR)$(includedir)/cloudi.h" \
                    "$(DESTDIR)$(prefix)/api/c/include/"
	list=`ls "$(DESTDIR)$(libdir)/" | grep 'libcloudi'`; for f in $$list; do \
        $(INSTALL_DATA) "$(DESTDIR)$(libdir)/$$f" \
                        "$(DESTDIR)$(prefix)/api/c/lib/"; \
        rm -f "$(DESTDIR)$(libdir)/$$f"; \
    done
	rm -f "$(DESTDIR)$(includedir)/cloudi.h"

api-java-install:
	$(MKDIR_P) "$(DESTDIR)$(prefix)/api/java"
	$(INSTALL_DATA) "$(DESTDIR)$(libexecdir)/cloudi.jar" \
                    "$(DESTDIR)$(prefix)/api/java/"
	rm -f "$(DESTDIR)$(libexecdir)/cloudi.jar"
	rmdir "$(DESTDIR)$(datarootdir)/java"

api-python-install:
	$(MKDIR_P) "$(DESTDIR)$(prefix)/api/python"
	$(INSTALL_DATA) "$(top_builddir)/api/python/erlang.py" \
                    "$(DESTDIR)$(prefix)/api/python/"
	$(INSTALL_DATA) "$(top_builddir)/api/python/cloudi.py" \
                    "$(DESTDIR)$(prefix)/api/python/"

api-ruby-install:
	$(MKDIR_P) "$(DESTDIR)$(prefix)/api/ruby"
	$(INSTALL_DATA) "$(top_builddir)/api/ruby/cloudi.rb" \
                    "$(DESTDIR)$(prefix)/api/ruby/"
	$(INSTALL_DATA) "$(top_builddir)/api/ruby/erlang.rb" \
                    "$(DESTDIR)$(prefix)/api/ruby/"

