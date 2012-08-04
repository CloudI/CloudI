ACLOCAL_AMFLAGS = -I m4
SUBDIRS = api lib tests

# automake expand all nested variables
rel/files/app.config: rel/files/app.config.in
	$(MKDIR_P) rel/files
	$(SED) -e "s![@]cloudi_confdir[@]!$(cloudi_confdir)!g" \
           -e "s![@]cloudi_logdir[@]!$(cloudi_logdir)!g" \
           $(top_srcdir)/rel/files/app.config.in > \
           $(top_builddir)/$@

all-local: rebar \
           rel/files/app.config
	$(MKDIR_P) external
	$(MKDIR_P) lib/cloudi/ebin
	$(MKDIR_P) lib/cpg/ebin
	$(MKDIR_P) lib/uuid/ebin
	$(MKDIR_P) lib/trie/ebin
	$(MKDIR_P) external/nodefinder/combonodefinder/ebin
	$(MKDIR_P) external/nodefinder/ec2nodefinder/ebin
	$(MKDIR_P) external/nodefinder/nodefinder/ebin
	$(MKDIR_P) external/misultin/ebin
	$(MKDIR_P) external/jsx/ebin
	$(MKDIR_P) external/etokyotyrant/ebin
	$(MKDIR_P) external/epgsql/ebin
	$(MKDIR_P) external/emysql/ebin
	$(MKDIR_P) external/ememcached/ebin
	$(MKDIR_P) external/ecouchdb/ebin
	$(MKDIR_P) external/zeromq/v$(ZEROMQ_VERSION_MAJOR)
	(cd $(top_srcdir)/lib/cloudi/src; \
        test ! -e cloudi.app.src && \
        $(LN_S) $(abs_top_builddir)/lib/cloudi/src/cloudi.app.src || exit 0)
	(cd $(top_srcdir)/lib/cpg/src; \
        test ! -e cpg.app.src && \
        $(LN_S) $(abs_top_builddir)/lib/cpg/src/cpg.app.src || exit 0)
	(cd $(top_srcdir)/lib/uuid/src; \
        test ! -e uuid.app.src && \
        $(LN_S) $(abs_top_builddir)/lib/uuid/src/uuid.app.src || exit 0)
	(cd $(top_srcdir)/lib/trie/src; \
        test ! -e trie.app.src && \
        $(LN_S) $(abs_top_builddir)/lib/trie/src/trie.app.src || exit 0)
	(cd $(top_srcdir)/lib/cloudi; \
        test ! -e ebin && \
        $(LN_S) $(abs_top_builddir)/lib/cloudi/ebin || exit 0)
	(cd $(top_srcdir)/external/nodefinder/nodefinder; \
        test ! -e ebin && $(LN_S) \
        $(abs_top_builddir)/external/nodefinder/nodefinder/ebin || \
        exit 0)
	(cd $(top_srcdir)/external/nodefinder/ec2nodefinder; \
        test ! -e ebin && $(LN_S) \
        $(abs_top_builddir)/external/nodefinder/ec2nodefinder/ebin || \
        exit 0)
	(cd $(top_srcdir)/external/nodefinder/combonodefinder; \
        test ! -e ebin && $(LN_S) \
        $(abs_top_builddir)/external/nodefinder/combonodefinder/ebin || \
        exit 0)
	(cd $(top_srcdir)/external/misultin; \
        test ! -e ebin && \
        $(LN_S) $(abs_top_builddir)/external/misultin/ebin || exit 0)
	(cd $(top_srcdir)/external/jsx; \
        test ! -e ebin && \
        $(LN_S) $(abs_top_builddir)/external/jsx/ebin || exit 0)
	(cd $(top_srcdir)/external/etokyotyrant; \
        test ! -e ebin && \
        $(LN_S) $(abs_top_builddir)/external/etokyotyrant/ebin || exit 0)
	(cd $(top_srcdir)/external/epgsql; \
        test ! -e ebin && \
        $(LN_S) $(abs_top_builddir)/external/epgsql/ebin || exit 0)
	(cd $(top_srcdir)/external/emysql; \
        test ! -e ebin && \
        $(LN_S) $(abs_top_builddir)/external/emysql/ebin || exit 0)
	(cd $(top_srcdir)/external/ememcached; \
        test ! -e ebin && \
        $(LN_S) $(abs_top_builddir)/external/ememcached/ebin || exit 0)
	(cd $(top_srcdir)/external/ecouchdb; \
        test ! -e ebin && \
        $(LN_S) $(abs_top_builddir)/external/ecouchdb/ebin || exit 0)
	(cd $(top_builddir)/external/zeromq/v$(ZEROMQ_VERSION_MAJOR); \
        test ! -e erlzmq && \
        $(LN_S) $(abs_top_srcdir)/external/zeromq/v$(ZEROMQ_VERSION_MAJOR)/erlzmq || exit 0)
	(cd $(top_srcdir) && \
     $(ESCRIPT) $(abs_top_builddir)/rebar \
                -C $(abs_top_builddir)/rebar.config compile || exit 0)

clean-local: rebar
	(cd $(top_srcdir) && \
     $(ESCRIPT) $(abs_top_builddir)/rebar \
                -C $(abs_top_builddir)/rebar.config clean || \
     exit 0)
	rmdir $(top_builddir)/lib/cloudi/ebin \
          $(top_builddir)/lib/cpg/ebin \
          $(top_builddir)/lib/uuid/ebin \
          $(top_builddir)/lib/trie/ebin \
          $(top_builddir)/external/nodefinder/nodefinder/ebin \
          $(top_builddir)/external/nodefinder/ec2nodefinder/ebin \
          $(top_builddir)/external/nodefinder/combonodefinder/ebin \
          $(top_builddir)/external/misultin/ebin \
          $(top_builddir)/external/etokyotyrant/ebin \
          $(top_builddir)/external/epgsql/ebin \
          $(top_builddir)/external/emysql/ebin \
          $(top_builddir)/external/ememcached/ebin \
          $(top_builddir)/external/ecouchdb/ebin \
          $(top_builddir)/external/jsx/ebin \
          || exit 0
	rm -f $(top_srcdir)/lib/cloudi/ebin \
          $(top_srcdir)/lib/cpg/ebin \
          $(top_srcdir)/lib/uuid/ebin \
          $(top_srcdir)/lib/trie/ebin \
          $(top_srcdir)/external/nodefinder/nodefinder/ebin \
          $(top_srcdir)/external/nodefinder/ec2nodefinder/ebin \
          $(top_srcdir)/external/nodefinder/combonodefinder/ebin \
          $(top_srcdir)/external/misultin/ebin \
          $(top_srcdir)/external/etokyotyrant/ebin \
          $(top_srcdir)/external/epgsql/ebin \
          $(top_srcdir)/external/emysql/ebin \
          $(top_srcdir)/external/ememcached/ebin \
          $(top_srcdir)/external/ecouchdb/ebin \
          $(top_srcdir)/external/jsx/ebin

clean-configure: config.log
	(cd $(top_srcdir)/external/zeromq/v$(ZEROMQ_VERSION_MAJOR)/zeromq && \
     make clean || exit 0)
	(cd $(top_srcdir)/external/zeromq/v$(ZEROMQ_VERSION_MAJOR)/erlzmq && \
     $(abs_top_builddir)/rebar -C $(abs_top_builddir)/rebar.config clean || \
     exit 0)
	rm -f $(top_srcdir)/aclocal.m4 \
          $(top_srcdir)/config.h.in \
          $(top_srcdir)/config.h.in~ \
          $(top_builddir)/config.h \
          $(top_builddir)/config.log \
          $(top_builddir)/config.status \
          $(top_builddir)/stamp-h1 \
          $(top_srcdir)/config/config.guess \
          $(top_srcdir)/config/config.sub \
          $(top_srcdir)/config/depcomp \
          $(top_srcdir)/config/install-sh \
          $(top_srcdir)/config/ltmain.sh \
          $(top_srcdir)/config/missing \
          $(top_builddir)/libtool \
          $(top_builddir)/rebar \
          $(top_builddir)/Makefile \
          $(top_builddir)/Makefile.in \
          $(top_srcdir)/configure \
          $(top_srcdir)/m4/libtool.m4 \
          $(top_srcdir)/m4/ltoptions.m4 \
          $(top_srcdir)/m4/ltsugar.m4 \
          $(top_srcdir)/m4/ltversion.m4 \
          $(top_srcdir)/m4/lt~obsolete.m4 \
          $(top_srcdir)/external/rebar/ebin/*.beam \
          $(top_builddir)/lib/cloudi/src/cloudi.app.src \
          $(top_builddir)/rel/files/app.config \
          $(top_builddir)/rebar.config \
          $(top_builddir)/reltool.config \
          $(top_builddir)/cloudi.conf
	rm -rf $(top_srcdir)/autom4te.cache
	rm -rf $(top_builddir)/api/c/.deps/ \
           $(top_builddir)/lib/cloudi/cxx_src/.deps/ \
           $(top_builddir)/tests/hexpi/cxx_src/.deps/ \
           $(top_builddir)/tests/http_req/c_src/.deps/ \
           $(top_builddir)/tests/flood/service/.deps/ \
           $(top_builddir)/tests/messaging/cxx_src/.deps/ \
           $(top_builddir)/tests/msg_size/cxx_src/.deps/
	find $(top_builddir)/api \
         -name "Makefile.in" -o -name "Makefile" | xargs rm -f
	find $(top_builddir)/lib \
         -name "Makefile.in" -o -name "Makefile" | xargs rm -f
	find $(top_builddir)/tests \
         -name "Makefile.in" -o -name "Makefile" | xargs rm -f

dialyze: all-local dialyzer.plt
	$(DIALYZER) --plt dialyzer.plt \
                -I lib/cloudi/include/ \
                lib/cloudi/ebin/ \
                lib/cpg/ebin/ \
                lib/uuid/ebin/ \
                lib/trie/ebin/ \
                external/epgsql/ebin \
                external/etokyotyrant/ebin \
                external/ememcached/ebin \
                external/ecouchdb/ebin \
                external/emysql/ebin \
                external/misultin/ebin \
                external/jsx/ebin

dialyzer.plt:
	$(DIALYZER) --build_plt --output_plt dialyzer.plt \
                --apps erts kernel stdlib mnesia crypto inets xmerl sasl \
                       compiler debugger ssl tools

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
                   tests-messaging-install \
                   tests-msg_size-install \
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
	$(ESCRIPT) $(abs_top_builddir)/rebar generate || exit 0
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
	$(INSTALL_SCRIPT) "$(top_srcdir)/tests/flood/service/flood.py" \
                      "$(DESTDIR)$(cloudi_prefix)/tests/flood/service/"
	$(INSTALL_DATA) "$(top_srcdir)/tests/flood/service/flood.rb" \
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
	$(INSTALL_SCRIPT) "$(top_srcdir)/tests/http/run.sh" \
                      "$(DESTDIR)$(cloudi_prefix)/tests/http/"
	$(INSTALL_DATA) "$(top_builddir)/tests/http/service/jar/cloudi.jar" \
                    "$(DESTDIR)$(cloudi_prefix)/tests/http/service/jar/"
	$(INSTALL_DATA) "$(top_builddir)/tests/http/service/jar/service.jar" \
                    "$(DESTDIR)$(cloudi_prefix)/tests/http/service/jar/"
	$(INSTALL_SCRIPT) "$(top_srcdir)/tests/http/service/service.py" \
                      "$(DESTDIR)$(cloudi_prefix)/tests/http/service/"
	$(INSTALL_DATA) "$(top_srcdir)/tests/http/service/service.rb" \
                    "$(DESTDIR)$(cloudi_prefix)/tests/http/service/"
	$(INSTALL_DATA) "$(top_srcdir)/tests/http/input/text" \
                    "$(DESTDIR)$(cloudi_prefix)/tests/http/input/"
	$(INSTALL_DATA) "$(top_srcdir)/tests/http/input.zip/text" \
                    "$(DESTDIR)$(cloudi_prefix)/tests/http/input.zip/"
	$(INSTALL_DATA) "$(top_srcdir)/tests/http/output/text" \
                    "$(DESTDIR)$(cloudi_prefix)/tests/http/output/"

tests-http_req-install: \
        $(top_builddir)/tests/http_req/c_src/http_req \
        $(top_builddir)/tests/http_req/jar/cloudi.jar \
        $(top_builddir)/tests/http_req/jar/http_req.jar \
        $(top_builddir)/tests/http_req/ebin/cloudi_job_http_req.beam
	$(MKDIR_P) "$(DESTDIR)$(cloudi_prefix)/tests/http_req/priv"
	$(MKDIR_P) "$(DESTDIR)$(cloudi_prefix)/tests/http_req/jar"
	$(MKDIR_P) "$(DESTDIR)$(cloudi_prefix)/tests/http_req/public_html"
	$(MKDIR_P) "$(DESTDIR)$(cloudi_prefix)/tests/http_req/ebin"
	$(INSTALL_PROGRAM) "$(top_builddir)/tests/http_req/c_src/http_req" \
                       "$(DESTDIR)$(cloudi_prefix)/tests/http_req/priv/"
	$(INSTALL_SCRIPT) "$(top_srcdir)/tests/http_req/http_req.py" \
                      "$(DESTDIR)$(cloudi_prefix)/tests/http_req/"
	$(INSTALL_SCRIPT) "$(top_srcdir)/tests/http_req/http_req.rb" \
                      "$(DESTDIR)$(cloudi_prefix)/tests/http_req/"
	$(INSTALL_DATA) "$(top_builddir)/tests/http_req/jar/cloudi.jar" \
                    "$(DESTDIR)$(cloudi_prefix)/tests/http_req/jar/"
	$(INSTALL_DATA) "$(top_builddir)/tests/http_req/jar/http_req.jar" \
                    "$(DESTDIR)$(cloudi_prefix)/tests/http_req/jar/"
	$(INSTALL_DATA) \
        "$(top_builddir)/tests/http_req/ebin/cloudi_job_http_req.beam" \
        "$(DESTDIR)$(cloudi_prefix)/tests/http_req/ebin/"
	$(INSTALL_DATA) "$(top_srcdir)/tests/http_req/public_html/index.html" \
                    "$(DESTDIR)$(cloudi_prefix)/tests/http_req/public_html/"
	$(INSTALL_DATA) \
        "$(top_srcdir)/tests/http_req/public_html/800px-Juliadim2.png" \
        "$(DESTDIR)$(cloudi_prefix)/tests/http_req/public_html/"

tests-job_api-install:
	$(MKDIR_P) "$(DESTDIR)$(cloudi_prefix)/tests/job_api"
	$(INSTALL_SCRIPT) "$(top_srcdir)/tests/job_api/run.py" \
                      "$(DESTDIR)$(cloudi_prefix)/tests/job_api/"
	$(INSTALL_SCRIPT) "$(top_srcdir)/tests/job_api/logging_on.py" \
                      "$(DESTDIR)$(cloudi_prefix)/tests/job_api/"
	$(INSTALL_SCRIPT) "$(top_srcdir)/tests/job_api/logging_off.py" \
                      "$(DESTDIR)$(cloudi_prefix)/tests/job_api/"
	$(INSTALL_SCRIPT) "$(top_srcdir)/tests/job_api/path.py" \
                      "$(DESTDIR)$(cloudi_prefix)/tests/job_api/"

tests-messaging-install: \
        $(top_builddir)/tests/messaging/ebin/cloudi_job_messaging_sequence1.beam \
        $(top_builddir)/tests/messaging/ebin/cloudi_job_messaging_sequence2.beam \
        $(top_builddir)/tests/messaging/ebin/cloudi_job_messaging_sequence3.beam \
        $(top_builddir)/tests/messaging/jar/cloudi.jar \
        $(top_builddir)/tests/messaging/jar/messaging.jar \
        $(top_builddir)/tests/messaging/cxx_src/messaging
	$(MKDIR_P) "$(DESTDIR)$(cloudi_prefix)/tests/messaging/ebin"
	$(MKDIR_P) "$(DESTDIR)$(cloudi_prefix)/tests/messaging/jar"
	$(MKDIR_P) "$(DESTDIR)$(cloudi_prefix)/tests/messaging/priv"
	$(INSTALL_DATA) \
        "$(top_builddir)/tests/messaging/ebin/cloudi_job_messaging_sequence1.beam" \
        "$(DESTDIR)$(cloudi_prefix)/tests/messaging/ebin/"
	$(INSTALL_DATA) \
        "$(top_builddir)/tests/messaging/ebin/cloudi_job_messaging_sequence2.beam" \
        "$(DESTDIR)$(cloudi_prefix)/tests/messaging/ebin/"
	$(INSTALL_DATA) \
        "$(top_builddir)/tests/messaging/ebin/cloudi_job_messaging_sequence3.beam" \
        "$(DESTDIR)$(cloudi_prefix)/tests/messaging/ebin/"
	$(INSTALL_DATA) "$(top_builddir)/tests/messaging/jar/cloudi.jar" \
                    "$(DESTDIR)$(cloudi_prefix)/tests/messaging/jar/"
	$(INSTALL_DATA) "$(top_builddir)/tests/messaging/jar/messaging.jar" \
                    "$(DESTDIR)$(cloudi_prefix)/tests/messaging/jar/"
	$(INSTALL_SCRIPT) "$(top_srcdir)/tests/messaging/messaging.py" \
                      "$(DESTDIR)$(cloudi_prefix)/tests/messaging/"
	$(INSTALL_SCRIPT) "$(top_srcdir)/tests/messaging/messaging.rb" \
                      "$(DESTDIR)$(cloudi_prefix)/tests/messaging/"
	$(INSTALL_PROGRAM) "$(top_builddir)/tests/messaging/cxx_src/messaging" \
                       "$(DESTDIR)$(cloudi_prefix)/tests/messaging/priv/"

tests-msg_size-install: \
        $(top_builddir)/tests/msg_size/ebin/cloudi_job_msg_size.beam \
        $(top_builddir)/tests/msg_size/jar/cloudi.jar \
        $(top_builddir)/tests/msg_size/jar/msg_size.jar \
        $(top_builddir)/tests/msg_size/cxx_src/msg_size
	$(MKDIR_P) "$(DESTDIR)$(cloudi_prefix)/tests/msg_size/ebin"
	$(MKDIR_P) "$(DESTDIR)$(cloudi_prefix)/tests/msg_size/jar"
	$(MKDIR_P) "$(DESTDIR)$(cloudi_prefix)/tests/msg_size/priv"
	$(INSTALL_DATA) \
        "$(top_builddir)/tests/msg_size/ebin/cloudi_job_msg_size.beam" \
        "$(DESTDIR)$(cloudi_prefix)/tests/msg_size/ebin/"
	$(INSTALL_DATA) "$(top_builddir)/tests/msg_size/jar/cloudi.jar" \
                    "$(DESTDIR)$(cloudi_prefix)/tests/msg_size/jar/"
	$(INSTALL_DATA) "$(top_builddir)/tests/msg_size/jar/msg_size.jar" \
                    "$(DESTDIR)$(cloudi_prefix)/tests/msg_size/jar/"
	$(INSTALL_SCRIPT) "$(top_srcdir)/tests/msg_size/msg_size.py" \
                      "$(DESTDIR)$(cloudi_prefix)/tests/msg_size/"
	$(INSTALL_SCRIPT) "$(top_srcdir)/tests/msg_size/msg_size.rb" \
                      "$(DESTDIR)$(cloudi_prefix)/tests/msg_size/"
	$(INSTALL_PROGRAM) "$(top_builddir)/tests/msg_size/cxx_src/msg_size" \
                       "$(DESTDIR)$(cloudi_prefix)/tests/msg_size/priv/"

tests-zeromq-install:
	$(MKDIR_P) "$(DESTDIR)$(cloudi_prefix)/tests/zeromq"
	$(INSTALL_SCRIPT) "$(top_srcdir)/tests/zeromq/run.py" \
                      "$(DESTDIR)$(cloudi_prefix)/tests/zeromq/"

api-install:
	$(MKDIR_P) "$(DESTDIR)$(cloudi_prefix)/api"

api-c-install:
	$(MKDIR_P) "$(DESTDIR)$(cloudi_prefix)/api/c"

api-java-install: $(top_builddir)/api/java/jar/cloudi.jar
	rmdir "$(DESTDIR)$(datarootdir)/java" || exit 0
	$(MKDIR_P) "$(DESTDIR)$(cloudi_prefix)/api/java"
	$(INSTALL_DATA) "$(top_builddir)/api/java/jar/cloudi.jar" \
                    "$(DESTDIR)$(cloudi_prefix)/api/java/"

api-python-install:
	$(MKDIR_P) "$(DESTDIR)$(cloudi_prefix)/api/python"
	$(INSTALL_DATA) "$(top_srcdir)/api/python/erlang.py" \
                    "$(DESTDIR)$(cloudi_prefix)/api/python/"
	$(INSTALL_DATA) "$(top_srcdir)/api/python/cloudi.py" \
                    "$(DESTDIR)$(cloudi_prefix)/api/python/"

api-ruby-install:
	$(MKDIR_P) "$(DESTDIR)$(cloudi_prefix)/api/ruby"
	$(INSTALL_DATA) "$(top_srcdir)/api/ruby/cloudi.rb" \
                    "$(DESTDIR)$(cloudi_prefix)/api/ruby/"
	$(INSTALL_DATA) "$(top_srcdir)/api/ruby/erlang.rb" \
                    "$(DESTDIR)$(cloudi_prefix)/api/ruby/"

job_api-install:
	$(MKDIR_P) "$(DESTDIR)$(cloudi_prefix)/job_api"

job_api-python-install:
	$(MKDIR_P) "$(DESTDIR)$(cloudi_prefix)/job_api/python/jsonrpclib/jsonrpclib"
	$(INSTALL_DATA) "$(top_srcdir)/job_api/python/cloudi_job_api.py" \
                    "$(DESTDIR)$(cloudi_prefix)/job_api/python/"
	$(INSTALL_DATA) "$(top_srcdir)/job_api/python/jsonrpclib/LICENSE.txt" \
                    "$(DESTDIR)$(cloudi_prefix)/job_api/python/jsonrpclib/"
	$(INSTALL_DATA) "$(top_srcdir)/job_api/python/jsonrpclib/README.md" \
                    "$(DESTDIR)$(cloudi_prefix)/job_api/python/jsonrpclib/"
	$(INSTALL_DATA) \
        "$(top_srcdir)/job_api/python/jsonrpclib/jsonrpclib/__init__.py" \
        "$(DESTDIR)$(cloudi_prefix)/job_api/python/jsonrpclib/jsonrpclib/"
	$(INSTALL_DATA) \
        "$(top_srcdir)/job_api/python/jsonrpclib/jsonrpclib/jsonrpc.py" \
        "$(DESTDIR)$(cloudi_prefix)/job_api/python/jsonrpclib/jsonrpclib/"
	$(INSTALL_DATA) \
        "$(top_srcdir)/job_api/python/jsonrpclib/jsonrpclib/config.py" \
        "$(DESTDIR)$(cloudi_prefix)/job_api/python/jsonrpclib/jsonrpclib/"
	$(INSTALL_DATA) \
        "$(top_srcdir)/job_api/python/jsonrpclib/jsonrpclib/history.py" \
        "$(DESTDIR)$(cloudi_prefix)/job_api/python/jsonrpclib/jsonrpclib/"
	$(INSTALL_DATA) \
        "$(top_srcdir)/job_api/python/jsonrpclib/jsonrpclib/jsonclass.py" \
        "$(DESTDIR)$(cloudi_prefix)/job_api/python/jsonrpclib/jsonrpclib/"

