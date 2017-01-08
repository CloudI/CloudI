#-*-Mode:make;coding:utf-8;tab-width:4;c-basic-offset:4-*-
# ex: set ft=make fenc=utf-8 sts=4 ts=4 sw=4 noet nomod:

ACLOCAL_AMFLAGS = -I m4
SUBDIRS = api lib tests service_api

SCOPE_EXTERNAL = cloudi_x_
SCOPE_ARGS = -s $(SCOPE_EXTERNAL) -p cloudi -b _original \
     -c $(top_srcdir)/lib/ \
     -d $(top_srcdir)/lib/cloudi_core/src \
     -d $(top_srcdir)/lib/cloudi_service_api_requests/src \
     -d $(top_srcdir)/lib/cloudi_service_db_mysql/src \
     -d $(top_srcdir)/lib/cloudi_service_db_pgsql/src \
     -d $(top_srcdir)/lib/cloudi_service_filesystem/src \
     -d $(top_srcdir)/lib/cloudi_service_http_client/src \
     -d $(top_srcdir)/lib/cloudi_service_http_cowboy/src \
     -d $(top_srcdir)/lib/cloudi_service_http_elli/src \
     -d $(top_srcdir)/lib/cloudi_service_http_rest/src \
     -d $(top_srcdir)/lib/cloudi_service_map_reduce/src \
     -d $(top_srcdir)/lib/cloudi_service_monitoring/src \
     -d $(top_srcdir)/lib/cloudi_service_oauth1/src \
     -d $(top_srcdir)/lib/cloudi_service_queue/src \
     -d $(top_srcdir)/lib/cloudi_service_quorum/src \
     -d $(top_srcdir)/lib/cloudi_service_router/src \
     -d $(top_srcdir)/lib/cloudi_service_tcp/src \
     -d $(top_srcdir)/lib/cloudi_service_udp/src \
     -d $(top_srcdir)/lib/cloudi_service_validate/src \
     -d $(top_srcdir)/lib/cloudi_service_zeromq/src \
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
     -d $(top_srcdir)/external/cloudi_x_cowboy/src \
     -d $(top_srcdir)/external/cloudi_x_cowlib/src \
     -d $(top_srcdir)/external/cloudi_x_elli/src \
     -d $(top_srcdir)/external/cloudi_x_emysql/src \
     -d $(top_srcdir)/external/cloudi_x_epgsql/src \
     -d $(top_srcdir)/external/cloudi_x_epgsql_wg/src \
     -d $(top_srcdir)/external/cloudi_x_exometer/src \
     -d $(top_srcdir)/external/cloudi_x_exometer_core/src \
     -d $(top_srcdir)/external/cloudi_x_folsom/src \
     -d $(top_srcdir)/external/cloudi_x_goldrush/src \
     -d $(top_srcdir)/external/cloudi_x_jsx/src \
     -d $(top_srcdir)/external/cloudi_x_lager/src \
     -d $(top_srcdir)/external/cloudi_x_msgpack/src \
     -d $(top_srcdir)/external/cloudi_x_nodefinder/src \
     -d $(top_srcdir)/external/cloudi_x_parse_trans/src \
     -d $(top_srcdir)/external/cloudi_x_pgsql/src \
     -d $(top_srcdir)/external/cloudi_x_ranch/src \
     -d $(top_srcdir)/external/cloudi_x_setup/src

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
    test \
    tests \
    api-python-test \
    api-ruby-test \
    run

all-local: rel/files/app.config \
           erlang-applications-scope-add \
           rebar-directories-compile \
           rebar-compile

rel/files/app.config: rel/files/app.config.in
	$(MKDIR_P) rel/files
	$(SED) -e "s![@]cloudi_confdir[@]!$(cloudi_confdir)!g" \
           -e "s![@]cloudi_logdir[@]!$(cloudi_logdir)!g" \
           $(top_srcdir)/rel/files/app.config.in > \
           $(top_builddir)/$@

rebar-directories-compile:
	$(top_srcdir)/rebar_dirs.sh create $(abs_top_srcdir) $(abs_top_builddir)
	(if test "x$(ZEROMQ_VERSION_MAJOR)" != "x"; then : ; \
        $(MKDIR_P) external/zeromq/v$(ZEROMQ_VERSION_MAJOR) && \
        cd $(top_builddir)/external/zeromq/v$(ZEROMQ_VERSION_MAJOR) && \
            test ! -e erlzmq && \
            $(LN_S) $(abs_top_srcdir)/external/zeromq/v$(ZEROMQ_VERSION_MAJOR)/erlzmq || exit 0; \
     fi)
	(cd $(top_srcdir)/lib/cloudi_core/cxx_src && \
        test ! -e cloudi_os_spawn_vsn_1 && \
        $(LN_S) $(abs_top_builddir)/lib/cloudi_core/cxx_src/cloudi_os_spawn_vsn_1 || exit 0)

erlang-applications-scope-add: scoped

scoped: $(top_srcdir)/external/README.markdown
	rm -f $@ ; echo > $@
	$(ESCRIPT) $(top_srcdir)/lib/reltool_util/scope $(SCOPE_ARGS) -u
	$(ESCRIPT) $(top_srcdir)/lib/reltool_util/scope $(SCOPE_ARGS)

rebar-compile: erlang-applications-scope-add \
               rebar-directories-compile
	(cd $(top_srcdir) && \
     $(ESCRIPT) $(REBAR) -C $(abs_top_builddir)/rebar_src.config compile)

clean-local: rebar-clean \
             rebar-directories-clean
	rm -rf $(top_builddir)/rel/cloudi || exit 0

rebar-clean:
	(cd $(top_srcdir) && \
     $(ESCRIPT) $(REBAR) -C $(abs_top_builddir)/rebar_src.config clean || \
     exit 0)

rebar-directories-clean: rebar-clean
	$(top_srcdir)/rebar_dirs.sh destroy $(abs_top_srcdir) $(abs_top_builddir)
	(if test "x$(ZEROMQ_VERSION_MAJOR)" != "x"; then : ; \
       test -d external/zeromq/v$(ZEROMQ_VERSION_MAJOR) && \
       rm -f $(top_builddir)/external/zeromq/v$(ZEROMQ_VERSION_MAJOR)/erlzmq \
             || exit 0; \
     fi)
	(test -h $(top_builddir)/lib/cloudi_core/cxx_src/cloudi_os_spawn_vsn_1 && \
	 rm $(top_builddir)/lib/cloudi_core/cxx_src/cloudi_os_spawn_vsn_1 || \
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
          $(top_srcdir)/m4/lt~obsolete.m4 \
          $(top_builddir)/rel/files/app.config
	rm -rf $(top_srcdir)/autom4te.cache
	rm -rf $(top_srcdir)/tests/.eunit \
           $(top_srcdir)/tests/ct/logs \
           $(top_srcdir)/lib/cloudi_core/test_ct/logs \
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
	(cd $(top_srcdir)/external/zeromq/v$(ZEROMQ_VERSION_MAJOR)/zeromq && \
     make distclean || exit 0)
	(cd $(top_srcdir)/external/zeromq/v$(ZEROMQ_VERSION_MAJOR)/erlzmq && \
     $(ESCRIPT) $(REBAR) -C $(abs_top_builddir)/rebar_src.config clean || \
     exit 0)
	test -e $(top_builddir)/rebar && rm -f $(top_builddir)/rebar || exit 0

erlang-applications-scope-remove:
	rm -f scoped
	$(ESCRIPT) $(top_srcdir)/lib/reltool_util/scope $(SCOPE_ARGS) -u

dialyze: all \
         $(top_srcdir)/dialyzer.plt
	$(DIALYZER) --verbose --plt $(top_srcdir)/dialyzer.plt \
              -I lib/cloudi_core/include/ \
              lib/cloudi_core/ebin/ \
              lib/cloudi_service_api_requests/ebin/ \
              lib/cloudi_service_db_mysql/ebin/ \
              lib/cloudi_service_db_pgsql/ebin/ \
              lib/cloudi_service_filesystem/ebin/ \
              lib/cloudi_service_http_client/ebin/ \
              lib/cloudi_service_http_cowboy/ebin/ \
              lib/cloudi_service_http_elli/ebin/ \
              lib/cloudi_service_http_rest/ebin/ \
              lib/cloudi_service_map_reduce/ebin/ \
              lib/cloudi_service_monitoring/ebin/ \
              lib/cloudi_service_oauth1/ebin/ \
              lib/cloudi_service_queue/ebin/ \
              lib/cloudi_service_quorum/ebin/ \
              lib/cloudi_service_router/ebin/ \
              lib/cloudi_service_tcp/ebin/ \
              lib/cloudi_service_udp/ebin/ \
              lib/cloudi_service_validate/ebin/ \
              lib/cloudi_service_zeromq/ebin/ \
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
              tests/hexpi/ebin/ \
              tests/request_rate/ebin/ \
              tests/messaging/ebin/ \
              tests/msg_size/ebin/ \
              tests/null/ebin/ \
              tests/http_req/ebin/ \
              external/cloudi_x_cowlib/ebin \
              external/cloudi_x_emysql/ebin \
              external/cloudi_x_epgsql/ebin \
              external/cloudi_x_epgsql_wg/ebin \
              external/cloudi_x_msgpack/ebin \
              external/cloudi_x_nodefinder/ebin
	@echo "No Errors or Warnings Found!"

dialyzer: dialyze

$(top_srcdir)/dialyzer.plt:
	$(DIALYZER) --build_plt --output_plt $(top_srcdir)/dialyzer.plt \
                --apps erts kernel stdlib mnesia crypto inets xmerl sasl \
                       compiler debugger ssl tools syntax_tools

doc: all
	(cd $(top_srcdir) && \
     $(ESCRIPT) $(REBAR) -C $(abs_top_builddir)/rebar_src_test.config doc \
        || exit 0)

docs: doc

xref: all
	(cd $(top_srcdir) && \
     $(ESCRIPT) $(REBAR) -C $(abs_top_builddir)/rebar_src_test.config xref \
        || exit 0)

api-python-test:
	$(PYTHON) $(top_srcdir)/api/python/tests/erlang_tests.py

api-ruby-test:
	$(RUBY) $(top_srcdir)/api/ruby/tests/erlang_tests.rb

ct: all
	(cd $(top_srcdir) && \
     $(ESCRIPT) $(REBAR) -C $(abs_top_builddir)/rebar_src_test.config ct)

eunit: all
	(cd $(top_srcdir) && \
     $(ESCRIPT) $(REBAR) -C $(abs_top_builddir)/rebar_src_test.config eunit)

test: all $(PYTHON_TEST_HOOK) $(RUBY_TEST_HOOK)
	(cd $(top_srcdir) && \
     $(ESCRIPT) $(REBAR) -C $(abs_top_builddir)/rebar_src_test.config ct eunit)

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

install-exec-hook: rel/files/app.config
	$(MKDIR_P) "$(DESTDIR)$(bindir)"
	$(MKDIR_P) "$(DESTDIR)$(cloudi_logdir)"
	$(MKDIR_P) "$(DESTDIR)$(cloudi_confdir)"
	$(MKDIR_P) "$(DESTDIR)$(cloudi_bindir)"
	$(MKDIR_P) "$(DESTDIR)$(cloudi_prefix)"
	$(MKDIR_P) "$(DESTDIR)$(cloudi_prefix)/lib"
	$(MKDIR_P) "$(DESTDIR)$(cloudi_prefix)/releases"
	(cd "$(DESTDIR)$(cloudi_prefix)" && \
     test ! -e etc && $(LN_S) "$(cloudi_confdir)" etc || exit 0 && \
     test ! -e logs && $(LN_S) "$(cloudi_logdir)" logs || exit 0)
	test -e "$(DESTDIR)$(cloudi_confdir)/cloudi.conf" && \
       mv -f "$(DESTDIR)$(cloudi_confdir)/cloudi.conf" \
             "$(DESTDIR)$(cloudi_confdir)/cloudi.conf.old" || exit 0
	$(INSTALL_DATA) "$(top_builddir)/$(CLOUDI_CONF)" \
                  "$(DESTDIR)$(cloudi_confdir)/cloudi.conf"
	$(ESCRIPT) $(top_srcdir)/lib/reltool_util/release
	test ! -d "$(DESTDIR)$(cloudi_prefix)/erts-$(ERLANG_ERTS_VER)" && \
       mv -f "$(top_builddir)/rel/cloudi/erts-$(ERLANG_ERTS_VER)" \
             "$(DESTDIR)$(cloudi_prefix)" || exit 0
	$(INSTALL_SCRIPT) "$(top_builddir)/rel/cloudi/bin/cloudi" \
                    "$(DESTDIR)$(cloudi_bindir)/"
	echo "#!/bin/sh"               > "$(CLOUDI_TMP_BIN_SCRIPT)"
	echo "cd $(cloudi_bindir)"     >> "$(CLOUDI_TMP_BIN_SCRIPT)"
	echo "exec ./cloudi \$$*"      >> "$(CLOUDI_TMP_BIN_SCRIPT)"
	$(INSTALL_SCRIPT) "$(CLOUDI_TMP_BIN_SCRIPT)" "$(DESTDIR)$(bindir)/cloudi"
	rm -f "$(CLOUDI_TMP_BIN_SCRIPT)"
	test -e "$(DESTDIR)$(cloudi_confdir)/app.config" && \
       mv -f "$(DESTDIR)$(cloudi_confdir)/app.config" \
             "$(DESTDIR)$(cloudi_confdir)/app.config.old" || exit 0
	$(INSTALL_DATA) "$(top_builddir)/rel/cloudi/etc/app.config" \
                  "$(DESTDIR)$(cloudi_confdir)/"
	test -e "$(DESTDIR)$(cloudi_confdir)/vm.args" && \
       mv -f "$(DESTDIR)$(cloudi_confdir)/vm.args" \
             "$(DESTDIR)$(cloudi_confdir)/vm.args.old" || exit 0
	$(INSTALL_DATA) "$(top_builddir)/rel/cloudi/etc/vm.args" \
                  "$(DESTDIR)$(cloudi_confdir)/"
	test ! -d "$(DESTDIR)$(cloudi_prefix)/releases/1" && \
       mv -f "$(top_builddir)/rel/cloudi/releases/1" \
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

