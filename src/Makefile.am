#-*-Mode:make;coding:utf-8;tab-width:4;c-basic-offset:4-*-
# ex: set ft=make fenc=utf-8 sts=4 ts=4 sw=4 noet nomod:

ACLOCAL_AMFLAGS = -I m4
SUBDIRS = api lib tests service_api

SCOPE_EXTERNAL = cloudi_x_
SCOPE_ARGS = -s $(SCOPE_EXTERNAL) -p cloudi -b _original \
     -c $(top_srcdir)/lib/ \
     -d $(top_srcdir)/lib/cloudi_core/src \
     -d $(top_srcdir)/lib/cloudi_service_api_batch/src \
     -d $(top_srcdir)/lib/cloudi_service_api_requests/src \
     -d $(top_srcdir)/lib/cloudi_service_cron/src \
     -d $(top_srcdir)/lib/cloudi_service_db_mysql/src \
     -d $(top_srcdir)/lib/cloudi_service_db_pgsql/src \
     -d $(top_srcdir)/lib/cloudi_service_filesystem/src \
     -d $(top_srcdir)/lib/cloudi_service_http_client/src \
     -d $(top_srcdir)/lib/cloudi_service_http_cowboy1/src \
     -d $(top_srcdir)/lib/cloudi_service_http_elli/src \
     -d $(top_srcdir)/lib/cloudi_service_http_rest/src \
     -d $(top_srcdir)/lib/cloudi_service_map_reduce/src \
     -d $(top_srcdir)/lib/cloudi_service_monitoring/src \
     -d $(top_srcdir)/lib/cloudi_service_null/src \
     -d $(top_srcdir)/lib/cloudi_service_oauth1/src \
     -d $(top_srcdir)/lib/cloudi_service_queue/src \
     -d $(top_srcdir)/lib/cloudi_service_quorum/src \
     -d $(top_srcdir)/lib/cloudi_service_request_rate/src \
     -d $(top_srcdir)/lib/cloudi_service_router/src \
     -d $(top_srcdir)/lib/cloudi_service_shell/src \
     -d $(top_srcdir)/lib/cloudi_service_tcp/src \
     -d $(top_srcdir)/lib/cloudi_service_udp/src \
     -d $(top_srcdir)/lib/cloudi_service_validate/src \
     -d $(top_srcdir)/lib/cgroups/src \
     -d $(top_srcdir)/lib/cpg/src \
     -d $(top_srcdir)/lib/cpg/test \
     -d $(top_srcdir)/lib/erlang_term/src \
     -d $(top_srcdir)/lib/key2value/src \
     -d $(top_srcdir)/lib/keys1value/src \
     -d $(top_srcdir)/lib/pqueue/src \
     -d $(top_srcdir)/lib/pqueue/test \
     -d $(top_srcdir)/lib/quickrand/src \
     -d $(top_srcdir)/lib/reltool_util/src \
     -d $(top_srcdir)/lib/supool/src \
     -d $(top_srcdir)/lib/supool/test \
     -d $(top_srcdir)/lib/syslog_socket/src \
     -d $(top_srcdir)/lib/syslog_socket/test \
     -d $(top_srcdir)/lib/trie/src \
     -d $(top_srcdir)/lib/uuid/src \
     -d $(top_srcdir)/lib/varpool/src \
     -d $(top_srcdir)/lib/varpool/test \
     -d $(top_srcdir)/external/cloudi_x_bear/src \
     -d $(top_srcdir)/external/cloudi_x_certifi/src \
     -d $(top_srcdir)/external/cloudi_x_cowboy1/src \
     -d $(top_srcdir)/external/cloudi_x_cowlib1/src \
     -d $(top_srcdir)/external/cloudi_x_elli/src \
     -d $(top_srcdir)/external/cloudi_x_emysql/src \
     -d $(top_srcdir)/external/cloudi_x_epgsql/src \
     -d $(top_srcdir)/external/cloudi_x_exometer/src \
     -d $(top_srcdir)/external/cloudi_x_exometer_core/src \
     -d $(top_srcdir)/external/cloudi_x_exometer_influxdb/src \
     -d $(top_srcdir)/external/cloudi_x_folsom/src \
     -d $(top_srcdir)/external/cloudi_x_hackney/src \
     -d $(top_srcdir)/external/cloudi_x_hut/src \
     -d $(top_srcdir)/external/cloudi_x_idna/src \
     -d $(top_srcdir)/external/cloudi_x_jsx/src \
     -d $(top_srcdir)/external/cloudi_x_metrics/src \
     -d $(top_srcdir)/external/cloudi_x_mimerl/src \
     -d $(top_srcdir)/external/cloudi_x_msgpack/src \
     -d $(top_srcdir)/external/cloudi_x_nodefinder/src \
     -d $(top_srcdir)/external/cloudi_x_parse_trans/src \
     -d $(top_srcdir)/external/cloudi_x_pgsql/src \
     -d $(top_srcdir)/external/cloudi_x_ranch1/src \
     -d $(top_srcdir)/external/cloudi_x_setup/src \
     -d $(top_srcdir)/external/cloudi_x_ssl_verify_fun/src \
     -d $(top_srcdir)/external/cloudi_x_unicode_util_compat/src

if PYTHON_SUPPORT
    PYTHON_TEST_HOOK = api-python-test
endif
if RUBY_SUPPORT
    RUBY_TEST_HOOK = api-ruby-test
endif

.PHONY: \
    rebar-directories-compile \
    erlang-applications-scope-add \
    rebar-compile \
    rebar-clean \
    rebar-directories-clean \
    clean-configure \
    rebar-directories-distclean \
    erlang-applications-scope-remove \
    dialyze \
    dialyzer \
    doc \
    docs \
    xref \
    api-python-test \
    api-ruby-test \
    ct \
    eunit \
    test \
    tests \
    manual-update \
    run

.NOTPARALLEL:

all-local: erlang-applications-scope-add \
           rebar-directories-compile \
           rebar-compile

rebar-directories-compile:
	$(top_srcdir)/rebar_dirs.sh create $(abs_top_srcdir) $(abs_top_builddir)
	(cd $(top_srcdir)/lib/cloudi_core/src && \
        test ! -e cloudi_environment.hrl && \
        $(LN_S) $(abs_top_builddir)/lib/cloudi_core/src/cloudi_environment.hrl || exit 0)
	(cd $(top_srcdir)/lib/cloudi_core/cxx_src && \
        test ! -e cloudi_os_spawn && \
        $(LN_S) $(abs_top_builddir)/lib/cloudi_core/cxx_src/cloudi_os_spawn || exit 0)

erlang-applications-scope-add: scoped

scoped: $(top_srcdir)/external/README.markdown
	rm -f $@ ; echo > $@
	$(ESCRIPT) $(top_srcdir)/lib/reltool_util/scope $(SCOPE_ARGS) -u
	$(ESCRIPT) $(top_srcdir)/lib/reltool_util/scope $(SCOPE_ARGS)

rebar-compile: erlang-applications-scope-add \
               rebar-directories-compile
	(cd $(top_srcdir) && \
     $(ESCRIPT) $(REBAR) $(REBAR_ARGS) \
                         -C $(abs_top_builddir)/rebar_src.config compile && \
	 $(top_srcdir)/rebar_dirs.sh process $(abs_top_srcdir) $(abs_top_builddir))

clean-local: rebar-clean \
             rebar-directories-clean
	rm -rf $(top_builddir)/rel/cloudi || exit 0

rebar-clean:
	(cd $(top_srcdir) && \
     $(ESCRIPT) $(REBAR) $(REBAR_ARGS) \
                         -C $(abs_top_builddir)/rebar_src.config clean || \
     exit 0)

rebar-directories-clean: rebar-clean
	$(top_srcdir)/rebar_dirs.sh destroy $(abs_top_srcdir) $(abs_top_builddir)
	(test -h $(top_builddir)/lib/cloudi_core/src/cloudi_environment.hrl && \
	 rm $(top_builddir)/lib/cloudi_core/src/cloudi_environment.hrl || \
	 exit 0)
	(test -h $(top_builddir)/lib/cloudi_core/cxx_src/cloudi_os_spawn && \
	 rm $(top_builddir)/lib/cloudi_core/cxx_src/cloudi_os_spawn || \
	 exit 0)

clean-configure: distclean

distclean-local: rebar-directories-distclean \
                 erlang-applications-scope-remove
	rm -f $(top_srcdir)/aclocal.m4 \
          $(top_srcdir)/config.h.in \
          $(top_srcdir)/config.h.in~ \
          $(top_srcdir)/config/compile \
          $(top_srcdir)/config/config.guess \
          $(top_srcdir)/config/config.sub \
          $(top_srcdir)/config/install-sh \
          $(top_srcdir)/config/ltmain.sh \
          $(top_srcdir)/config/missing \
          $(top_srcdir)/Makefile.in \
          $(top_srcdir)/configure \
          $(top_srcdir)/m4/libtool.m4 \
          $(top_srcdir)/m4/ltoptions.m4 \
          $(top_srcdir)/m4/ltsugar.m4 \
          $(top_srcdir)/m4/ltversion.m4 \
          $(top_srcdir)/m4/lt~obsolete.m4
	rm -rf $(top_srcdir)/autom4te.cache
	rm -rf $(top_srcdir)/tests/.eunit \
           $(top_srcdir)/tests/ct/logs \
           $(top_srcdir)/lib/cloudi_core/test_ct/logs \
           $(top_srcdir)/lib/cloudi_service_api_batch/test_ct/logs \
           $(top_srcdir)/lib/cloudi_service_db_mysql/test_ct/logs \
           $(top_srcdir)/lib/cloudi_service_db_pgsql/test_ct/logs \
           $(top_srcdir)/lib/cloudi_service_filesystem/test_ct/logs \
           $(top_srcdir)/lib/cloudi_service_http_client/test_ct/logs \
           $(top_srcdir)/lib/cloudi_service_queue/test_ct/logs \
           $(top_srcdir)/lib/cloudi_service_quorum/test_ct/logs \
           $(top_srcdir)/lib/cloudi_service_oauth1/test_ct/logs \
           $(top_srcdir)/lib/cloudi_service_validate/test_ct/logs
	find $(top_srcdir)/api -name "Makefile.in" | xargs rm -f
	find $(top_srcdir)/lib -name "Makefile.in" | xargs rm -f
	find $(top_srcdir)/tests -name "Makefile.in" | xargs rm -f
	find $(top_srcdir)/service_api -name "Makefile.in" | xargs rm -f

rebar-directories-distclean:
	rm -f $(top_srcdir)/external/rebar/ebin/*.beam
	test -e $(top_builddir)/rebar && rm -f $(top_builddir)/rebar || exit 0

erlang-applications-scope-remove:
	rm -f scoped
	$(ESCRIPT) $(top_srcdir)/lib/reltool_util/scope $(SCOPE_ARGS) -u

dialyze: all \
         $(top_srcdir)/dialyzer.plt
	$(DIALYZER) --verbose --plt $(top_srcdir)/dialyzer.plt \
              -I lib/cloudi_core/include/ \
              lib/cloudi_core/ebin/ \
              lib/cloudi_service_api_batch/ebin/ \
              lib/cloudi_service_api_requests/ebin/ \
              lib/cloudi_service_cron/ebin/ \
              lib/cloudi_service_db_mysql/ebin/ \
              lib/cloudi_service_db_pgsql/ebin/ \
              lib/cloudi_service_filesystem/ebin/ \
              lib/cloudi_service_http_client/ebin/ \
              lib/cloudi_service_http_cowboy1/ebin/ \
              lib/cloudi_service_http_elli/ebin/ \
              lib/cloudi_service_http_rest/ebin/ \
              lib/cloudi_service_map_reduce/ebin/ \
              lib/cloudi_service_monitoring/ebin/ \
              lib/cloudi_service_null/ebin/ \
              lib/cloudi_service_oauth1/ebin/ \
              lib/cloudi_service_queue/ebin/ \
              lib/cloudi_service_quorum/ebin/ \
              lib/cloudi_service_request_rate/ebin/ \
              lib/cloudi_service_router/ebin/ \
              lib/cloudi_service_shell/ebin/ \
              lib/cloudi_service_tcp/ebin/ \
              lib/cloudi_service_udp/ebin/ \
              lib/cloudi_service_validate/ebin/ \
              lib/cgroups/ebin/ \
              lib/cpg/ebin/ \
              lib/erlang_term/ebin/ \
              lib/key2value/ebin/ \
              lib/keys1value/ebin/ \
              lib/pqueue/ebin/ \
              lib/quickrand/ebin/ \
              lib/reltool_util/ebin/ \
              lib/supool/ebin/ \
              lib/syslog_socket/ebin/ \
              lib/trie/ebin/ \
              lib/uuid/ebin/ \
              lib/varpool/ebin/ \
              tests/count/erlang/src/ \
              tests/hexpi/erlang/src/ \
              tests/http_req/erlang/src/ \
              tests/messaging/erlang/src/ \
              tests/msg_size/erlang/src/ \
              tests/null/erlang/src/ \
              external/cloudi_x_bear/ebin \
              external/cloudi_x_cowlib1/ebin \
              external/cloudi_x_emysql/ebin \
              external/cloudi_x_epgsql/ebin \
              external/cloudi_x_folsom/ebin \
              external/cloudi_x_msgpack/ebin \
              external/cloudi_x_nodefinder/ebin \
              external/cloudi_x_pgsql/ebin \
              external/cloudi_x_setup/ebin
	@echo "No Errors or Warnings Found!"

dialyzer: dialyze

$(top_srcdir)/dialyzer.plt:
	$(DIALYZER) --build_plt --output_plt $(top_srcdir)/dialyzer.plt \
                --apps erts kernel stdlib mnesia crypto inets xmerl sasl \
                       compiler debugger ssl tools syntax_tools \
                       public_key ssh

doc: all
	(cd $(top_srcdir) && \
     $(ESCRIPT) $(REBAR) $(REBAR_ARGS) \
                         -C $(abs_top_builddir)/rebar_src_test.config doc \
        || exit 0)

docs: doc

xref: all
	(cd $(top_srcdir) && \
     $(ESCRIPT) $(REBAR) $(REBAR_ARGS) \
                         -C $(abs_top_builddir)/rebar_src_test.config xref \
        || exit 0)

api-python-test:
	$(PYTHON) $(top_srcdir)/api/python/tests/erlang_tests.py

api-ruby-test:
	$(RUBY) $(top_srcdir)/api/ruby/tests/erlang_tests.rb

pylint:
	$(PYLINT) -rn --min-similarity-lines=40 \
              $(top_srcdir)/api/python/cloudi.py \
              $(top_srcdir)/api/python/cloudi_c.py \
              $(top_srcdir)/api/python/erlang.py \
              $(top_srcdir)/api/python/tests/erlang_tests.py \
              $(top_srcdir)/service_api/python/cloudi_service_api.py \
              $(top_srcdir)/tests/count/count.py \
              $(top_srcdir)/tests/count/count_c.py \
              $(top_srcdir)/tests/echo/echo.py \
              $(top_srcdir)/tests/environment/environment.py \
              $(top_srcdir)/tests/http/http.py \
              $(top_srcdir)/tests/http_req/http_req.py \
              $(top_srcdir)/tests/http_req/http_req_c.py \
              $(top_srcdir)/tests/messaging/messaging.py \
              $(top_srcdir)/tests/messaging/messaging_c.py \
              $(top_srcdir)/tests/msg_size/msg_size.py \
              $(top_srcdir)/tests/msg_size/msg_size_c.py \
              $(top_srcdir)/tests/null/null.py \
              $(top_srcdir)/tests/null/null_c.py \
              $(top_srcdir)/tests/service_api/logging_off.py \
              $(top_srcdir)/tests/service_api/logging_on.py \
              $(top_srcdir)/tests/service_api/path.py \
              $(top_srcdir)/tests/service_api/run.py \
              $(top_srcdir)/tests/websockets/websockets.py

linter-ruby:
	$(RUBY) -wc \
            $(top_srcdir)/api/ruby/erlang.rb \
            $(top_srcdir)/api/ruby/cloudi.rb \
            $(top_srcdir)/api/ruby/tests/erlang_tests.rb \
            $(top_srcdir)/tests/count/count.rb \
            $(top_srcdir)/tests/http/http.rb \
            $(top_srcdir)/tests/http_req/http_req.rb \
            $(top_srcdir)/tests/msg_size/msg_size.rb \
            $(top_srcdir)/tests/messaging/messaging.rb \
            $(top_srcdir)/tests/null/null.rb

ct: all
	(cd $(top_srcdir) && \
     $(ESCRIPT) $(REBAR) $(REBAR_ARGS) \
                         -C $(abs_top_builddir)/rebar_src_test.config ct)

eunit: all
	(cd $(top_srcdir) && \
     $(ESCRIPT) $(REBAR) $(REBAR_ARGS) \
                         -C $(abs_top_builddir)/rebar_src_test.config eunit)

test: all $(PYTHON_TEST_HOOK) $(RUBY_TEST_HOOK)
	(cd $(top_srcdir) && \
     $(ESCRIPT) $(REBAR) $(REBAR_ARGS) \
                         -C $(abs_top_builddir)/rebar_src_test.config ct eunit)

tests: test

manual-update:
	(cd $(top_srcdir) && \
     $(ESCRIPT) $(top_srcdir)/lib/reltool_util/ex2erl \
         $(top_srcdir)/lib/cloudi_core/priv/CloudILogger.ex && \
     mv -f $(top_srcdir)/lib/cloudi_core/priv/Elixir.CloudILogger.erl \
           $(top_srcdir)/lib/cloudi_core/src/ )
	(cd $(top_srcdir)/lib/cloudi_core/scripts/ && \
     $(ESCRIPT) ./mimetypes_update )

uninstall:
	rm -rf "$(DESTDIR)$(cloudi_prefix)"

install-exec-hook:
	$(MKDIR_P) "$(DESTDIR)$(bindir)"
	$(MKDIR_P) "$(DESTDIR)$(cloudi_logdir)"
	$(MKDIR_P) "$(DESTDIR)$(cloudi_confdir)"
	$(MKDIR_P) "$(DESTDIR)$(cloudi_bindir)"
	$(MKDIR_P) "$(DESTDIR)$(cloudi_prefix)"
	$(INSTALL_DATA) "$(top_srcdir)/LICENSE" "$(DESTDIR)$(cloudi_prefix)"
	$(MKDIR_P) "$(DESTDIR)$(cloudi_prefix)/lib"
	$(MKDIR_P) "$(DESTDIR)$(cloudi_prefix)/releases"
	(cd "$(DESTDIR)$(cloudi_prefix)" && \
     test ! -e etc && $(LN_S) "$(cloudi_confdir)" etc || exit 0 && \
     test ! -e logs && $(LN_S) "$(cloudi_logdir)" logs || exit 0)
	$(ESCRIPT) $(top_srcdir)/lib/reltool_util/release
	test ! -d "$(DESTDIR)$(cloudi_prefix)/erts-$(ERLANG_ERTS_VER)" && \
       mv -f "$(top_builddir)/rel/cloudi/erts-$(ERLANG_ERTS_VER)" \
             "$(DESTDIR)$(cloudi_prefix)" || exit 0
	$(INSTALL) -m 700 "$(top_builddir)/rel/cloudi/bin/nodetool" \
                      "$(DESTDIR)$(cloudi_bindir)/"
	$(INSTALL) -m 700 "$(top_builddir)/rel/cloudi/bin/cloudi" \
                      "$(DESTDIR)$(cloudi_bindir)/"
	echo "#!/bin/sh"               > "$(CLOUDI_TMP_BIN_SCRIPT)"
	echo "cd $(cloudi_bindir)"     >> "$(CLOUDI_TMP_BIN_SCRIPT)"
	echo "exec ./cloudi \$$*"      >> "$(CLOUDI_TMP_BIN_SCRIPT)"
	$(INSTALL) -m 700 "$(CLOUDI_TMP_BIN_SCRIPT)" "$(DESTDIR)$(bindir)/cloudi"
	rm -f "$(CLOUDI_TMP_BIN_SCRIPT)"
	test -e "$(DESTDIR)$(cloudi_confdir)/vm.args" && \
       mv -f "$(DESTDIR)$(cloudi_confdir)/vm.args" \
             "$(DESTDIR)$(cloudi_confdir)/vm.args.old" || exit 0
	$(INSTALL) -m 600 "$(top_builddir)/rel/cloudi/etc/vm.args" \
                      "$(DESTDIR)$(cloudi_confdir)/"
	test -e "$(DESTDIR)$(cloudi_confdir)/$(CLOUDI_NODE_NAME).args" && \
       mv -f "$(DESTDIR)$(cloudi_confdir)/$(CLOUDI_NODE_NAME).args" \
             "$(DESTDIR)$(cloudi_confdir)/$(CLOUDI_NODE_NAME).args.old" || exit 0
	$(INSTALL) -m 600 "$(top_builddir)/rel/cloudi/etc/$(CLOUDI_NODE_NAME).args" \
                      "$(DESTDIR)$(cloudi_confdir)/"
	test -e "$(DESTDIR)$(cloudi_confdir)/app.config" && \
       mv -f "$(DESTDIR)$(cloudi_confdir)/app.config" \
             "$(DESTDIR)$(cloudi_confdir)/app.config.old" || exit 0
	test -e "$(DESTDIR)$(cloudi_confdir)/vm.config" && \
       mv -f "$(DESTDIR)$(cloudi_confdir)/vm.config" \
             "$(DESTDIR)$(cloudi_confdir)/vm.config.old" || exit 0
	$(INSTALL) -m 600 "$(top_builddir)/rel/cloudi/etc/vm.config" \
                      "$(DESTDIR)$(cloudi_confdir)/"
	test -e "$(DESTDIR)$(cloudi_confdir)/cloudi.conf" && \
       mv -f "$(DESTDIR)$(cloudi_confdir)/cloudi.conf" \
             "$(DESTDIR)$(cloudi_confdir)/cloudi.conf.old" || exit 0
	$(INSTALL) -m 600 "$(top_builddir)/rel/cloudi/etc/cloudi.conf" \
                      "$(DESTDIR)$(cloudi_confdir)/"
	test ! -d "$(DESTDIR)$(cloudi_prefix)/releases/$(CLOUDI_CORE_RELEASE_VERSION)" && \
       mv -f "$(top_builddir)/rel/cloudi/releases/$(CLOUDI_CORE_RELEASE_VERSION)" \
             "$(DESTDIR)$(cloudi_prefix)/releases/" || exit 0
	$(INSTALL_DATA) "$(top_builddir)/rel/cloudi/releases/start_erl.data" \
                    "$(DESTDIR)$(cloudi_prefix)/releases/"
	list0=`ls "$(top_builddir)/rel/cloudi/lib/"`; \
    for subdir0 in $$list0; do \
	  list1=`ls "$(top_builddir)/rel/cloudi/lib/$$subdir0"`; \
    for subdir1 in $$list1; do \
        test ! -e "$(DESTDIR)$(cloudi_prefix)/lib/$$subdir0/$$subdir1" && \
        $(MKDIR_P) "$(DESTDIR)$(cloudi_prefix)/lib/$$subdir0/" && \
        mv -f "$(top_builddir)/rel/cloudi/lib/$$subdir0/$$subdir1" \
              "$(DESTDIR)$(cloudi_prefix)/lib/$$subdir0/" || \
        echo "$(DESTDIR)$(cloudi_prefix)/lib/$$subdir0/$$subdir1 exists!"; \
    done; done
	rm -rf $(top_builddir)/rel/cloudi/

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

