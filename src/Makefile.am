ACLOCAL_AMFLAGS = -I m4
SUBDIRS = api lib tests
CLOUDI_SCRIPT=$(DESTDIR)$(bindir)/cloudi

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
          lib/cloudi/src/cloudi.app.src config/sys.config \
          reltool.config cloudi.conf
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

run:
	@echo
	@echo "********************"
	@echo "* USE make install *"
	@echo "********************"
	@echo
	@exit 1

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

install-exec-hook: cloudi-prefix-install \
                   tests-install \
                   tests-flood-install \
                   tests-hexpi-install \
                   tests-http-install \
                   tests-http_req-install \
                   tests-job_api-install \
                   tests-zeromq-install \
                   api-install \
                   api-c-install \
                   api-java-install \
                   api-python-install \
                   api-ruby-install \
                   job_api-install \
                   job_api-python-install
	$(MKDIR_P) "$(DESTDIR)$(cloudi_logdir)"
	$(MKDIR_P) "$(DESTDIR)$(cloudi_confdir)"
	$(MKDIR_P) "$(DESTDIR)$(cloudi_bindir)"
	$(MKDIR_P) "$(DESTDIR)$(cloudi_prefix)/lib"
	$(MKDIR_P) "$(DESTDIR)$(cloudi_prefix)/releases"
	(cd "$(DESTDIR)$(cloudi_prefix)"; \
     test ! -e etc && $(LN_S) "$(cloudi_confdir)" etc || exit 0; \
     test ! -e logs && $(LN_S) "$(cloudi_logdir)" logs || exit 0)
	$(INSTALL_DATA) "$(top_builddir)/cloudi.conf" \
                    "$(DESTDIR)$(cloudi_confdir)"
	$(ESCRIPT) rebar generate || exit 0
	test ! -d "$(DESTDIR)$(cloudi_prefix)/erts-$(ERLANG_ERTS_VER)" && \
        mv -f "$(top_builddir)/rel/cloudi/erts-$(ERLANG_ERTS_VER)" \
              "$(DESTDIR)$(cloudi_prefix)" || exit 0
	$(INSTALL_SCRIPT) "$(top_builddir)/rel/cloudi/bin/cloudi" \
                      "$(DESTDIR)$(cloudi_bindir)/"
	echo "#!/bin/bash"              > /tmp/cloudi
	echo "cd $(cloudi_bindir)"     >> /tmp/cloudi
	echo "exec ./cloudi \$$*"      >> /tmp/cloudi
	$(INSTALL_SCRIPT) /tmp/cloudi "$(DESTDIR)$(bindir)/"
	$(INSTALL_DATA) "$(top_builddir)/rel/cloudi/etc/app.config" \
                    "$(DESTDIR)$(cloudi_confdir)/"
	$(INSTALL_DATA) "$(top_builddir)/rel/cloudi/etc/vm.args" \
                    "$(DESTDIR)$(cloudi_confdir)/"
	test ! -d "$(DESTDIR)$(cloudi_prefix)/releases/1" && \
        mv -f "$(top_builddir)/rel/cloudi/releases/1" \
              "$(DESTDIR)$(cloudi_prefix)/releases/" || exit 0
	$(INSTALL_DATA) "$(top_builddir)/rel/cloudi/releases/start_erl.data" \
                    "$(DESTDIR)$(cloudi_prefix)/releases/"
	list=`ls "$(top_builddir)/rel/cloudi/lib/"`; for subdir in $$list; do \
        test ! -e "$(DESTDIR)$(cloudi_prefix)/lib/$$subdir" && \
        mv -f "$(top_builddir)/rel/cloudi/lib/$$subdir" \
              "$(DESTDIR)$(cloudi_prefix)/lib/" || exit 0; \
    done
	rm -rf $(top_builddir)/rel/cloudi/

cloudi-prefix-install:
	$(MKDIR_P) "$(DESTDIR)$(bindir)"
	$(MKDIR_P) "$(DESTDIR)$(cloudi_prefix)"

tests-install:
	$(MKDIR_P) "$(DESTDIR)$(cloudi_prefix)/tests"

tests-flood-install: $(top_builddir)/tests/flood/service/flood \
                     $(top_builddir)/tests/flood/ebin/cloudi_job_flood.beam \
                     $(top_builddir)/tests/flood/service/jar/cloudi.jar \
                     $(top_builddir)/tests/flood/service/jar/flood.jar
	$(MKDIR_P) "$(DESTDIR)$(cloudi_prefix)/tests/flood/ebin"
	$(MKDIR_P) "$(DESTDIR)$(cloudi_prefix)/tests/flood/service"
	$(MKDIR_P) "$(DESTDIR)$(cloudi_prefix)/tests/flood/service/jar"
	$(INSTALL_DATA) "$(top_builddir)/tests/flood/ebin/cloudi_job_flood.beam" \
                    "$(DESTDIR)$(cloudi_prefix)/tests/flood/ebin/"
	$(INSTALL_PROGRAM) "$(top_builddir)/tests/flood/service/flood" \
                       "$(DESTDIR)$(cloudi_prefix)/tests/flood/service/"
	$(INSTALL_DATA) "$(top_builddir)/tests/flood/service/jar/cloudi.jar" \
                    "$(DESTDIR)$(cloudi_prefix)/tests/flood/service/jar/"
	$(INSTALL_DATA) "$(top_builddir)/tests/flood/service/jar/flood.jar" \
                    "$(DESTDIR)$(cloudi_prefix)/tests/flood/service/jar/"
	$(INSTALL_SCRIPT) "$(top_builddir)/tests/flood/service/flood.py" \
                      "$(DESTDIR)$(cloudi_prefix)/tests/flood/service/"
	$(INSTALL_DATA) "$(top_builddir)/tests/flood/service/flood.rb" \
                    "$(DESTDIR)$(cloudi_prefix)/tests/flood/service/"

tests-hexpi-install: $(top_builddir)/tests/hexpi/ebin/cloudi_job_hexpi.beam \
                     $(top_builddir)/tests/hexpi/cxx_src/hexpi
	$(MKDIR_P) "$(DESTDIR)$(cloudi_prefix)/tests/hexpi/ebin"
	$(MKDIR_P) "$(DESTDIR)$(cloudi_prefix)/tests/hexpi/priv"
	$(INSTALL_DATA) "$(top_builddir)/tests/hexpi/ebin/cloudi_job_hexpi.beam" \
                    "$(DESTDIR)$(cloudi_prefix)/tests/hexpi/ebin/"
	$(INSTALL_PROGRAM) "$(top_builddir)/tests/hexpi/cxx_src/hexpi" \
                       "$(DESTDIR)$(cloudi_prefix)/tests/hexpi/priv/"

tests-http-install: $(top_builddir)/tests/http/service/jar/cloudi.jar \
                    $(top_builddir)/tests/http/service/jar/service.jar
	$(MKDIR_P) "$(DESTDIR)$(cloudi_prefix)/tests/http/service/jar"
	$(MKDIR_P) "$(DESTDIR)$(cloudi_prefix)/tests/http/input"
	$(MKDIR_P) "$(DESTDIR)$(cloudi_prefix)/tests/http/input.zip"
	$(MKDIR_P) "$(DESTDIR)$(cloudi_prefix)/tests/http/output"
	$(MKDIR_P) "$(DESTDIR)$(cloudi_prefix)/tests/http/tmp"
	$(INSTALL_SCRIPT) "$(top_builddir)/tests/http/run.sh" \
                      "$(DESTDIR)$(cloudi_prefix)/tests/http/"
	$(INSTALL_DATA) "$(top_builddir)/tests/http/service/jar/cloudi.jar" \
                    "$(DESTDIR)$(cloudi_prefix)/tests/http/service/jar/"
	$(INSTALL_DATA) "$(top_builddir)/tests/http/service/jar/service.jar" \
                    "$(DESTDIR)$(cloudi_prefix)/tests/http/service/jar/"
	$(INSTALL_SCRIPT) "$(top_builddir)/tests/http/service/service.py" \
                      "$(DESTDIR)$(cloudi_prefix)/tests/http/service/"
	$(INSTALL_DATA) "$(top_builddir)/tests/http/service/service.rb" \
                    "$(DESTDIR)$(cloudi_prefix)/tests/http/service/"
	$(INSTALL_DATA) "$(top_builddir)/tests/http/input/text" \
                    "$(DESTDIR)$(cloudi_prefix)/tests/http/input/"
	$(INSTALL_DATA) "$(top_builddir)/tests/http/input.zip/text" \
                    "$(DESTDIR)$(cloudi_prefix)/tests/http/input.zip/"
	$(INSTALL_DATA) "$(top_builddir)/tests/http/output/text" \
                    "$(DESTDIR)$(cloudi_prefix)/tests/http/output/"

tests-http_req-install: $(top_builddir)/tests/http_req/c_src/http_req
	$(MKDIR_P) "$(DESTDIR)$(cloudi_prefix)/tests/http_req/priv"
	$(MKDIR_P) "$(DESTDIR)$(cloudi_prefix)/tests/http_req/public_html"
	$(INSTALL_PROGRAM) "$(top_builddir)/tests/http_req/c_src/http_req" \
                       "$(DESTDIR)$(cloudi_prefix)/tests/http_req/priv/"
	$(INSTALL_DATA) "$(top_builddir)/tests/http_req/public_html/index.html" \
                    "$(DESTDIR)$(cloudi_prefix)/tests/http_req/public_html/"
	$(INSTALL_DATA) \
        "$(top_builddir)/tests/http_req/public_html/800px-Juliadim2.png" \
        "$(DESTDIR)$(cloudi_prefix)/tests/http_req/public_html/"

tests-job_api-install:
	$(MKDIR_P) "$(DESTDIR)$(cloudi_prefix)/tests/job_api"
	$(INSTALL_SCRIPT) "$(top_builddir)/tests/job_api/run.py" \
                      "$(DESTDIR)$(cloudi_prefix)/tests/job_api/"

tests-zeromq-install:
	$(MKDIR_P) "$(DESTDIR)$(cloudi_prefix)/tests/zeromq"
	$(INSTALL_SCRIPT) "$(top_builddir)/tests/zeromq/run.py" \
                      "$(DESTDIR)$(cloudi_prefix)/tests/zeromq/"

api-install:
	$(MKDIR_P) "$(DESTDIR)$(cloudi_prefix)/api"

api-c-install: $(top_builddir)/api/c/cloudi.h
	$(MKDIR_P) "$(DESTDIR)$(cloudi_prefix)/api/c/include"
	$(MKDIR_P) "$(DESTDIR)$(cloudi_prefix)/api/c/lib"
	$(INSTALL_DATA) "$(top_builddir)/api/c/cloudi.h" \
                    "$(DESTDIR)$(cloudi_prefix)/api/c/include/"
	list=`ls "$(top_builddir)/api/c/" | grep '^libcloudi'`; \
    for f in $$list; do \
        $(INSTALL_DATA) "$(top_builddir)/api/c/$$f" \
                        "$(DESTDIR)$(cloudi_prefix)/api/c/lib/"; \
    done

#rmdir "$(DESTDIR)$(datarootdir)/java" || exit 0
api-java-install: $(top_builddir)/api/java/jar/cloudi.jar
	$(MKDIR_P) "$(DESTDIR)$(cloudi_prefix)/api/java"
	$(INSTALL_DATA) "$(top_builddir)/api/java/jar/cloudi.jar" \
                    "$(DESTDIR)$(cloudi_prefix)/api/java/"

api-python-install:
	$(MKDIR_P) "$(DESTDIR)$(cloudi_prefix)/api/python"
	$(INSTALL_DATA) "$(top_builddir)/api/python/erlang.py" \
                    "$(DESTDIR)$(cloudi_prefix)/api/python/"
	$(INSTALL_DATA) "$(top_builddir)/api/python/cloudi.py" \
                    "$(DESTDIR)$(cloudi_prefix)/api/python/"

api-ruby-install:
	$(MKDIR_P) "$(DESTDIR)$(cloudi_prefix)/api/ruby"
	$(INSTALL_DATA) "$(top_builddir)/api/ruby/cloudi.rb" \
                    "$(DESTDIR)$(cloudi_prefix)/api/ruby/"
	$(INSTALL_DATA) "$(top_builddir)/api/ruby/erlang.rb" \
                    "$(DESTDIR)$(cloudi_prefix)/api/ruby/"

job_api-install:
	$(MKDIR_P) "$(DESTDIR)$(cloudi_prefix)/job_api"

job_api-python-install:
	$(MKDIR_P) "$(DESTDIR)$(cloudi_prefix)/job_api/python/jsonrpclib/jsonrpclib"
	$(INSTALL_DATA) "$(top_builddir)/job_api/python/cloudi_job_api.py" \
                    "$(DESTDIR)$(cloudi_prefix)/job_api/python/"
	$(INSTALL_DATA) "$(top_builddir)/job_api/python/jsonrpclib/LICENSE.txt" \
                    "$(DESTDIR)$(cloudi_prefix)/job_api/python/jsonrpclib/"
	$(INSTALL_DATA) "$(top_builddir)/job_api/python/jsonrpclib/README.md" \
                    "$(DESTDIR)$(cloudi_prefix)/job_api/python/jsonrpclib/"
	$(INSTALL_DATA) \
        "$(top_builddir)/job_api/python/jsonrpclib/jsonrpclib/__init__.py" \
        "$(DESTDIR)$(cloudi_prefix)/job_api/python/jsonrpclib/jsonrpclib/"
	$(INSTALL_DATA) \
        "$(top_builddir)/job_api/python/jsonrpclib/jsonrpclib/jsonrpc.py" \
        "$(DESTDIR)$(cloudi_prefix)/job_api/python/jsonrpclib/jsonrpclib/"
	$(INSTALL_DATA) \
        "$(top_builddir)/job_api/python/jsonrpclib/jsonrpclib/config.py" \
        "$(DESTDIR)$(cloudi_prefix)/job_api/python/jsonrpclib/jsonrpclib/"
	$(INSTALL_DATA) \
        "$(top_builddir)/job_api/python/jsonrpclib/jsonrpclib/history.py" \
        "$(DESTDIR)$(cloudi_prefix)/job_api/python/jsonrpclib/jsonrpclib/"
	$(INSTALL_DATA) \
        "$(top_builddir)/job_api/python/jsonrpclib/jsonrpclib/jsonclass.py" \
        "$(DESTDIR)$(cloudi_prefix)/job_api/python/jsonrpclib/jsonrpclib/"

