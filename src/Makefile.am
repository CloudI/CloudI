#-*-Mode:make;coding:utf-8;tab-width:4;c-basic-offset:4-*-
# ex: set ft=make fenc=utf-8 sts=4 ts=4 sw=4 noet:
ACLOCAL_AMFLAGS = -I m4
SUBDIRS = api lib tests

SCOPE_EXTERNAL = cloudi_x_
SCOPE_ARGS = -s $(SCOPE_EXTERNAL) -p cloudi -b _original \
     -c $(top_srcdir)/lib/ \
     -d $(top_srcdir)/lib/cloudi_core/src \
     -d $(top_srcdir)/lib/cloudi_services_databases/src \
     -d $(top_srcdir)/lib/cloudi_services_internal/src \
     -d $(top_srcdir)/lib/cloudi_services_messaging/src \
     -d $(top_srcdir)/lib/cpg/src \
     -d $(top_srcdir)/lib/cpg/test \
     -d $(top_srcdir)/lib/key2value/src \
     -d $(top_srcdir)/lib/pqueue/src \
     -d $(top_srcdir)/lib/pqueue/test \
     -d $(top_srcdir)/lib/quickrand/src \
     -d $(top_srcdir)/lib/reltool_util/src \
     -d $(top_srcdir)/lib/trie/src \
     -d $(top_srcdir)/lib/uuid/src \
     -d $(top_srcdir)/external/nodefinder/combonodefinder/src \
     -d $(top_srcdir)/external/nodefinder/ec2nodefinder/src \
     -d $(top_srcdir)/external/nodefinder/nodefinder/src \
     -d $(top_srcdir)/external/cowboy/src \
     -d $(top_srcdir)/external/cowboy/test \
     -d $(top_srcdir)/external/elli/src \
     -d $(top_srcdir)/external/elli/test \
     -d $(top_srcdir)/external/dynamic_compile/src \
     -d $(top_srcdir)/external/ecouchdb/src \
     -d $(top_srcdir)/external/ememcached/src \
     -d $(top_srcdir)/external/emysql/src \
     -d $(top_srcdir)/external/epgsql/src \
     -d $(top_srcdir)/external/erlasticsearch/src \
     -d $(top_srcdir)/external/etokyotyrant/src \
     -d $(top_srcdir)/external/jsx/src \
     -d $(top_srcdir)/external/poolboy/src \
     -d $(top_srcdir)/external/poolboy/test \
     -d $(top_srcdir)/external/ranch/src \
     -d $(top_srcdir)/external/ranch/test \
     -d $(top_srcdir)/external/thrift/src \
     -d $(top_srcdir)/external/thrift/test

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
	$(MKDIR_P) external
	$(MKDIR_P) lib/cloudi_core/ebin
	$(MKDIR_P) lib/cloudi_services_internal/ebin
	$(MKDIR_P) lib/cloudi_services_databases/ebin
	$(MKDIR_P) lib/cloudi_services_messaging/ebin
	$(MKDIR_P) lib/pqueue/ebin
	$(MKDIR_P) lib/quickrand/ebin
	$(MKDIR_P) lib/cpg/ebin
	$(MKDIR_P) lib/uuid/ebin
	$(MKDIR_P) lib/trie/ebin
	$(MKDIR_P) lib/reltool_util/ebin
	$(MKDIR_P) lib/key2value/ebin
	$(MKDIR_P) external/nodefinder/combonodefinder/ebin
	$(MKDIR_P) external/nodefinder/ec2nodefinder/ebin
	$(MKDIR_P) external/nodefinder/nodefinder/ebin
	$(MKDIR_P) external/jsx/ebin
	$(MKDIR_P) external/etokyotyrant/ebin
	$(MKDIR_P) external/poolboy/ebin
	$(MKDIR_P) external/thrift/ebin
	$(MKDIR_P) external/erlasticsearch/ebin
	$(MKDIR_P) external/epgsql/ebin
	$(MKDIR_P) external/emysql/ebin
	$(MKDIR_P) external/ememcached/ebin
	$(MKDIR_P) external/ecouchdb/ebin
	$(MKDIR_P) external/cowboy/ebin
	$(MKDIR_P) external/ranch/ebin
	$(MKDIR_P) external/elli/ebin
	$(MKDIR_P) external/dynamic_compile/ebin
	(cd $(top_srcdir)/lib/cloudi_core/src && \
        test ! -e cloudi_core.app.src && \
        $(LN_S) $(abs_top_builddir)/lib/cloudi_core/src/cloudi_core.app.src || exit 0)
	(cd $(top_srcdir)/lib/cloudi_services_internal/src && \
        test ! -e cloudi_services_internal.app.src && \
        $(LN_S) $(abs_top_builddir)/lib/cloudi_services_internal/src/cloudi_services_internal.app.src || exit 0)
	(cd $(top_srcdir)/lib/cloudi_services_databases/src && \
        test ! -e cloudi_services_databases.app.src && \
        $(LN_S) $(abs_top_builddir)/lib/cloudi_services_databases/src/cloudi_services_databases.app.src || exit 0)
	(cd $(top_srcdir)/lib/cloudi_services_messaging/src && \
        test ! -e cloudi_services_messaging.app.src && \
        $(LN_S) $(abs_top_builddir)/lib/cloudi_services_messaging/src/cloudi_services_messaging.app.src || exit 0)
	(cd $(top_srcdir)/lib/pqueue/src && \
        test ! -e $(SCOPE_EXTERNAL)pqueue.app.src && \
        $(LN_S) $(abs_top_builddir)/lib/pqueue/src/$(SCOPE_EXTERNAL)pqueue.app.src || exit 0)
	(cd $(top_srcdir)/lib/quickrand/src && \
        test ! -e $(SCOPE_EXTERNAL)quickrand.app.src && \
        $(LN_S) $(abs_top_builddir)/lib/quickrand/src/$(SCOPE_EXTERNAL)quickrand.app.src || exit 0)
	(cd $(top_srcdir)/lib/cpg/src && \
        test ! -e $(SCOPE_EXTERNAL)cpg.app.src && \
        $(LN_S) $(abs_top_builddir)/lib/cpg/src/$(SCOPE_EXTERNAL)cpg.app.src || exit 0)
	(cd $(top_srcdir)/lib/uuid/src && \
        test ! -e $(SCOPE_EXTERNAL)uuid.app.src && \
        $(LN_S) $(abs_top_builddir)/lib/uuid/src/$(SCOPE_EXTERNAL)uuid.app.src || exit 0)
	(cd $(top_srcdir)/lib/trie/src && \
        test ! -e $(SCOPE_EXTERNAL)trie.app.src && \
        $(LN_S) $(abs_top_builddir)/lib/trie/src/$(SCOPE_EXTERNAL)trie.app.src || exit 0)
	(cd $(top_srcdir)/lib/reltool_util/src && \
        test ! -e $(SCOPE_EXTERNAL)reltool_util.app.src && \
        $(LN_S) $(abs_top_builddir)/lib/reltool_util/src/$(SCOPE_EXTERNAL)reltool_util.app.src || exit 0)
	(cd $(top_srcdir)/lib/key2value/src && \
        test ! -e $(SCOPE_EXTERNAL)key2value.app.src && \
        $(LN_S) $(abs_top_builddir)/lib/key2value/src/$(SCOPE_EXTERNAL)key2value.app.src || exit 0)
	(cd $(top_srcdir)/lib/cloudi_core && \
        test ! -e ebin && \
        $(LN_S) $(abs_top_builddir)/lib/cloudi_core/ebin || exit 0)
	(cd $(top_srcdir)/lib/cloudi_services_internal && \
        test ! -e ebin && \
        $(LN_S) $(abs_top_builddir)/lib/cloudi_services_internal/ebin || exit 0)
	(cd $(top_srcdir)/lib/cloudi_services_databases && \
        test ! -e ebin && \
        $(LN_S) $(abs_top_builddir)/lib/cloudi_services_databases/ebin || exit 0)
	(cd $(top_srcdir)/lib/cloudi_services_messaging && \
        test ! -e ebin && \
        $(LN_S) $(abs_top_builddir)/lib/cloudi_services_messaging/ebin || exit 0)
	(cd $(top_srcdir)/lib/pqueue && \
        test ! -e ebin && \
        $(LN_S) $(abs_top_builddir)/lib/pqueue/ebin || exit 0)
	(cd $(top_srcdir)/lib/quickrand && \
        test ! -e ebin && \
        $(LN_S) $(abs_top_builddir)/lib/quickrand/ebin || exit 0)
	(cd $(top_srcdir)/lib/cpg && \
        test ! -e ebin && \
        $(LN_S) $(abs_top_builddir)/lib/cpg/ebin || exit 0)
	(cd $(top_srcdir)/lib/uuid && \
        test ! -e ebin && \
        $(LN_S) $(abs_top_builddir)/lib/uuid/ebin || exit 0)
	(cd $(top_srcdir)/lib/trie && \
        test ! -e ebin && \
        $(LN_S) $(abs_top_builddir)/lib/trie/ebin || exit 0)
	(cd $(top_srcdir)/lib/reltool_util && \
        test ! -e ebin && \
        $(LN_S) $(abs_top_builddir)/lib/reltool_util/ebin || exit 0)
	(cd $(top_srcdir)/lib/key2value && \
        test ! -e ebin && \
        $(LN_S) $(abs_top_builddir)/lib/key2value/ebin || exit 0)
	(if test "x$(ZEROMQ_VERSION_MAJOR)" != "x"; then : ; \
       $(MKDIR_P) external/zeromq/v$(ZEROMQ_VERSION_MAJOR) && \
       cd $(top_builddir)/external/zeromq/v$(ZEROMQ_VERSION_MAJOR) && \
        test ! -e erlzmq && \
        $(LN_S) $(abs_top_srcdir)/external/zeromq/v$(ZEROMQ_VERSION_MAJOR)/erlzmq || exit 0; \
   fi)
	(cd $(top_srcdir)/external/nodefinder/nodefinder && \
        test ! -e ebin && $(LN_S) \
        $(abs_top_builddir)/external/nodefinder/nodefinder/ebin || \
        exit 0)
	(cd $(top_srcdir)/external/nodefinder/ec2nodefinder && \
        test ! -e ebin && $(LN_S) \
        $(abs_top_builddir)/external/nodefinder/ec2nodefinder/ebin || \
        exit 0)
	(cd $(top_srcdir)/external/nodefinder/combonodefinder && \
        test ! -e ebin && $(LN_S) \
        $(abs_top_builddir)/external/nodefinder/combonodefinder/ebin || \
        exit 0)
	(cd $(top_srcdir)/external/jsx && \
        test ! -e ebin && \
        $(LN_S) $(abs_top_builddir)/external/jsx/ebin || exit 0)
	(cd $(top_srcdir)/external/etokyotyrant && \
        test ! -e ebin && \
        $(LN_S) $(abs_top_builddir)/external/etokyotyrant/ebin || exit 0)
	(cd $(top_srcdir)/external/poolboy; \
        test ! -e ebin && \
        $(LN_S) $(abs_top_builddir)/external/poolboy/ebin || exit 0)
	(cd $(top_srcdir)/external/thrift; \
        test ! -e ebin && \
        $(LN_S) $(abs_top_builddir)/external/thrift/ebin || exit 0)
	(cd $(top_srcdir)/external/erlasticsearch; \
        test ! -e ebin && \
        $(LN_S) $(abs_top_builddir)/external/erlasticsearch/ebin || exit 0)
	(cd $(top_srcdir)/external/epgsql; \
        test ! -e ebin && \
        $(LN_S) $(abs_top_builddir)/external/epgsql/ebin || exit 0)
	(cd $(top_srcdir)/external/emysql && \
        test ! -e ebin && \
        $(LN_S) $(abs_top_builddir)/external/emysql/ebin || exit 0)
	(cd $(top_srcdir)/external/ememcached && \
        test ! -e ebin && \
        $(LN_S) $(abs_top_builddir)/external/ememcached/ebin || exit 0)
	(cd $(top_srcdir)/external/ecouchdb && \
        test ! -e ebin && \
        $(LN_S) $(abs_top_builddir)/external/ecouchdb/ebin || exit 0)
	(cd $(top_srcdir)/external/cowboy && \
        test ! -e ebin && \
        $(LN_S) $(abs_top_builddir)/external/cowboy/ebin || exit 0)
	(cd $(top_srcdir)/external/ranch && \
        test ! -e ebin && \
        $(LN_S) $(abs_top_builddir)/external/ranch/ebin || exit 0)
	(cd $(top_srcdir)/external/elli && \
        test ! -e ebin && \
        $(LN_S) $(abs_top_builddir)/external/elli/ebin || exit 0)
	(cd $(top_srcdir)/external/dynamic_compile && \
        test ! -e ebin && \
        $(LN_S) $(abs_top_builddir)/external/dynamic_compile/ebin || exit 0)
	(cd $(top_builddir)/external/epgsql && \
        test ! -e include && \
        $(LN_S) $(abs_top_srcdir)/external/epgsql/include || exit 0)
	(cd $(top_builddir)/external/epgsql && \
        test ! -e include && \
        $(LN_S) $(abs_top_srcdir)/external/epgsql/include || exit 0)
	(cd $(top_builddir)/external/emysql && \
        test ! -e include && \
        $(LN_S) $(abs_top_srcdir)/external/emysql/include || exit 0)
	(cd $(top_builddir)/external/elli && \
        test ! -e include && \
        $(LN_S) $(abs_top_srcdir)/external/elli/include || exit 0)
	(cd $(top_builddir)/lib/uuid && \
        test ! -e include && \
        $(LN_S) $(abs_top_srcdir)/lib/uuid/include || exit 0)
	(cd $(top_builddir)/lib/cpg && \
        test ! -e include && \
        $(LN_S) $(abs_top_srcdir)/lib/cpg/include || exit 0)
	(cd $(top_builddir)/lib/cloudi_core && \
        test ! -e include && \
        $(LN_S) $(abs_top_srcdir)/lib/cloudi_core/include || exit 0)

erlang-applications-scope-add:
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
	rmdir $(top_builddir)/lib/cloudi_core/ebin \
          $(top_builddir)/lib/cloudi_services_internal/ebin \
          $(top_builddir)/lib/cloudi_services_databases/ebin \
          $(top_builddir)/lib/cloudi_services_messaging/ebin \
          $(top_builddir)/lib/pqueue/ebin \
          $(top_builddir)/lib/quickrand/ebin \
          $(top_builddir)/lib/cpg/ebin \
          $(top_builddir)/lib/uuid/ebin \
          $(top_builddir)/lib/trie/ebin \
          $(top_builddir)/lib/reltool_util/ebin \
          $(top_builddir)/lib/key2value/ebin \
          $(top_builddir)/external/nodefinder/nodefinder/ebin \
          $(top_builddir)/external/nodefinder/ec2nodefinder/ebin \
          $(top_builddir)/external/nodefinder/combonodefinder/ebin \
          $(top_builddir)/external/jsx/ebin \
          $(top_builddir)/external/etokyotyrant/ebin \
          $(top_builddir)/external/poolboy/ebin \
          $(top_builddir)/external/thrift/ebin \
          $(top_builddir)/external/erlasticsearch/ebin \
          $(top_builddir)/external/epgsql/ebin \
          $(top_builddir)/external/emysql/ebin \
          $(top_builddir)/external/ememcached/ebin \
          $(top_builddir)/external/ecouchdb/ebin \
          $(top_builddir)/external/cowboy/ebin \
          $(top_builddir)/external/ranch/ebin \
          $(top_builddir)/external/elli/ebin \
          $(top_builddir)/external/dynamic_compile/ebin \
          || exit 0
	rm -f $(top_srcdir)/lib/cloudi_core/ebin \
          $(top_srcdir)/lib/cloudi_services_internal/ebin \
          $(top_srcdir)/lib/cloudi_services_databases/ebin \
          $(top_srcdir)/lib/cloudi_services_messaging/ebin \
          $(top_srcdir)/lib/pqueue/ebin \
          $(top_srcdir)/lib/quickrand/ebin \
          $(top_srcdir)/lib/cpg/ebin \
          $(top_srcdir)/lib/uuid/ebin \
          $(top_srcdir)/lib/trie/ebin \
          $(top_srcdir)/lib/reltool_util/ebin \
          $(top_srcdir)/lib/key2value/ebin \
          $(top_srcdir)/external/nodefinder/nodefinder/ebin \
          $(top_srcdir)/external/nodefinder/ec2nodefinder/ebin \
          $(top_srcdir)/external/nodefinder/combonodefinder/ebin \
          $(top_srcdir)/external/jsx/ebin \
          $(top_srcdir)/external/etokyotyrant/ebin \
          $(top_srcdir)/external/poolboy/ebin \
          $(top_srcdir)/external/thrift/ebin \
          $(top_srcdir)/external/erlasticsearch/ebin \
          $(top_srcdir)/external/epgsql/ebin \
          $(top_srcdir)/external/emysql/ebin \
          $(top_srcdir)/external/ememcached/ebin \
          $(top_srcdir)/external/ecouchdb/ebin \
          $(top_srcdir)/external/cowboy/ebin \
          $(top_srcdir)/external/ranch/ebin \
          $(top_srcdir)/external/elli/ebin \
          $(top_srcdir)/external/dynamic_compile/ebin

clean-configure: config.log \
                 clean \
                 rebar-directories-clean-configure \
                 erlang-applications-scope-remove
	(cd $(top_srcdir)/external/zeromq/v$(ZEROMQ_VERSION_MAJOR)/zeromq && \
     make clean || exit 0)
	(cd $(top_srcdir)/external/zeromq/v$(ZEROMQ_VERSION_MAJOR)/erlzmq && \
     $(ESCRIPT) $(REBAR) -C $(abs_top_builddir)/rebar_src.config clean || \
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
        $(top_srcdir)/Makefile.in \
        $(top_builddir)/Makefile \
        $(top_srcdir)/configure \
        $(top_srcdir)/m4/libtool.m4 \
        $(top_srcdir)/m4/ltoptions.m4 \
        $(top_srcdir)/m4/ltsugar.m4 \
        $(top_srcdir)/m4/ltversion.m4 \
        $(top_srcdir)/m4/lt~obsolete.m4 \
        $(top_builddir)/rel/files/app.config \
        $(top_builddir)/rebar_src.config \
        $(top_builddir)/rebar_src_test.config \
        $(top_builddir)/reltool.config \
        $(top_builddir)/cloudi.conf
	rm -rf $(top_srcdir)/autom4te.cache \
           $(top_srcdir)/tests/.eunit \
           $(top_srcdir)/tests/ct/logs \
           $(top_srcdir)/lib/cloudi_services_databases/test_ct/logs
	rm -rf $(top_builddir)/api/c/.deps/ \
         $(top_builddir)/api/python/.deps/ \
         $(top_builddir)/lib/cloudi_core/cxx_src/.deps/ \
         $(top_builddir)/tests/hexpi/cxx_src/.deps/ \
         $(top_builddir)/tests/http_req/c_src/.deps/ \
         $(top_builddir)/tests/flood/service/.deps/ \
         $(top_builddir)/tests/messaging/cxx_src/.deps/ \
         $(top_builddir)/tests/msg_size/cxx_src/.deps/
	find $(top_srcdir)/api -name "Makefile.in" | xargs rm -f
	find $(top_srcdir)/lib -name "Makefile.in" | xargs rm -f
	find $(top_srcdir)/tests -name "Makefile.in" | xargs rm -f
	find $(top_builddir)/api -name "Makefile" | xargs rm -f
	find $(top_builddir)/lib -name "Makefile" | xargs rm -f
	find $(top_builddir)/tests -name "Makefile" | xargs rm -f

rebar-directories-clean-configure:
	rm -f $(top_srcdir)/external/rebar/ebin/*.beam \
        $(top_srcdir)/lib/cloudi_core/src/cloudi_core.app.src \
        $(top_builddir)/lib/cloudi_core/src/cloudi_core.app.src \
        $(top_srcdir)/lib/cloudi_services_internal/src/cloudi_services_internal.app.src \
        $(top_builddir)/lib/cloudi_services_internal/src/cloudi_services_internal.app.src \
        $(top_srcdir)/lib/cloudi_services_databases/src/cloudi_services_databases.app.src \
        $(top_builddir)/lib/cloudi_services_databases/src/cloudi_services_databases.app.src \
        $(top_srcdir)/lib/cloudi_services_messaging/src/cloudi_services_messaging.app.src \
        $(top_builddir)/lib/cloudi_services_messaging/src/cloudi_services_messaging.app.src
	test -e $(top_builddir)/rebar && rm -f $(top_builddir)/rebar || exit 0

erlang-applications-scope-remove:
	$(ESCRIPT) $(top_srcdir)/lib/reltool_util/scope $(SCOPE_ARGS) -u

dialyze: all \
         dialyzer.plt
	$(DIALYZER) --plt dialyzer.plt \
              -Wno_return -Wno_unused \
              -I lib/cloudi_core/include/ \
              lib/cloudi_core/ebin/ \
              lib/cloudi_services_internal/ebin/ \
              lib/cloudi_services_databases/ebin/ \
              lib/cloudi_services_messaging/ebin/ \
              lib/pqueue/ebin/ \
              lib/quickrand/ebin/ \
              lib/cpg/ebin/ \
              lib/uuid/ebin/ \
              lib/trie/ebin/ \
              lib/reltool_util/ebin/ \
              lib/key2value/ebin/ \
              external/cowboy/ebin \
              external/elli/ebin \
              external/dynamic_compile/ebin \
              external/etokyotyrant/ebin \
              external/erlasticsearch/ebin \
              external/epgsql/ebin \
              external/ememcached/ebin \
              external/ecouchdb/ebin \
              external/emysql/ebin \
              external/jsx/ebin

dialyzer.plt:
	$(DIALYZER) --build_plt --output_plt dialyzer.plt \
                --apps erts kernel stdlib mnesia crypto inets xmerl sasl \
                       compiler debugger ssl tools

doc: all
	(cd $(top_srcdir) && \
     $(ESCRIPT) $(REBAR) -C $(abs_top_builddir)/rebar_src_test.config doc \
        || exit 0)

xref: all
	(cd $(top_srcdir) && \
     $(ESCRIPT) $(REBAR) -C $(abs_top_builddir)/rebar_src_test.config xref \
        || exit 0)

test: all
	(cd $(top_srcdir) && \
     $(ESCRIPT) $(REBAR) -C $(abs_top_builddir)/rebar_src_test.config ct && \
     $(ESCRIPT) $(REBAR) -C $(abs_top_builddir)/rebar_src_test.config eunit \
        || exit 0)

uninstall:
	rm -rf "$(DESTDIR)$(cloudi_prefix)"

install-exec-hook: cloudi-prefix-install \
                   tests-install \
                   tests-echo-install \
                   tests-environment-install \
                   tests-flood-install \
                   tests-hexpi-install \
                   tests-http-install \
                   tests-http_req-install \
                   tests-service_api-install \
                   tests-messaging-install \
                   tests-msg_size-install \
                   tests-websockets-install \
                   tests-zeromq-install \
                   api-install \
                   api-erlang-install \
                   api-c-install \
                   api-java-install \
                   api-python-install \
                   api-ruby-install \
                   service_api-install \
                   service_api-python-install \
                   rel/files/app.config
	$(MKDIR_P) "$(DESTDIR)$(cloudi_logdir)"
	$(MKDIR_P) "$(DESTDIR)$(cloudi_confdir)"
	$(MKDIR_P) "$(DESTDIR)$(cloudi_bindir)"
	$(MKDIR_P) "$(DESTDIR)$(cloudi_prefix)/lib"
	$(MKDIR_P) "$(DESTDIR)$(cloudi_prefix)/releases"
	(cd "$(DESTDIR)$(cloudi_prefix)" && \
     test ! -e etc && $(LN_S) "$(cloudi_confdir)" etc || exit 0 && \
     test ! -e logs && $(LN_S) "$(cloudi_logdir)" logs || exit 0)
	test -e "$(DESTDIR)$(cloudi_confdir)/cloudi.conf" && \
       mv -f "$(DESTDIR)$(cloudi_confdir)/cloudi.conf" \
             "$(DESTDIR)$(cloudi_confdir)/cloudi.conf.old" || exit 0
	$(INSTALL_DATA) "$(top_builddir)/cloudi.conf" \
                  "$(DESTDIR)$(cloudi_confdir)"
	$(ESCRIPT) $(top_srcdir)/lib/reltool_util/release
	test ! -d "$(DESTDIR)$(cloudi_prefix)/erts-$(ERLANG_ERTS_VER)" && \
       mv -f "$(top_builddir)/rel/cloudi/erts-$(ERLANG_ERTS_VER)" \
             "$(DESTDIR)$(cloudi_prefix)" || exit 0
	$(INSTALL_SCRIPT) "$(top_builddir)/rel/cloudi/bin/cloudi" \
                    "$(DESTDIR)$(cloudi_bindir)/"
	echo "#!/bin/sh"               > /tmp/cloudi
	echo "cd $(cloudi_bindir)"     >> /tmp/cloudi
	echo "exec ./cloudi \$$*"      >> /tmp/cloudi
	$(INSTALL_SCRIPT) /tmp/cloudi "$(DESTDIR)$(bindir)/"
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
	$(LIBTOOL) --mode=finish "$(DESTDIR)$(cloudi_prefix)/api/c"
	$(LIBTOOL) --mode=finish --quiet "$(DESTDIR)$(cloudi_prefix)/api/python"
	$(LIBTOOL) --mode=finish --quiet "$(DESTDIR)$(cloudi_prefix)/lib/cloudi_core-$(VERSION)/priv"

cloudi-prefix-install:
	$(MKDIR_P) "$(DESTDIR)$(bindir)"
	$(MKDIR_P) "$(DESTDIR)$(cloudi_prefix)"

tests-install:
	$(MKDIR_P) "$(DESTDIR)$(cloudi_prefix)/tests"

tests-echo-install:
	$(MKDIR_P) "$(DESTDIR)$(cloudi_prefix)/tests/echo"
	$(INSTALL_SCRIPT) "$(top_srcdir)/tests/echo/echo.py" \
                      "$(DESTDIR)$(cloudi_prefix)/tests/echo/"

tests-environment-install:
	$(MKDIR_P) "$(DESTDIR)$(cloudi_prefix)/tests/environment"
	$(INSTALL_SCRIPT) "$(top_srcdir)/tests/environment/environment.py" \
                      "$(DESTDIR)$(cloudi_prefix)/tests/environment/"

tests-flood-install: $(top_builddir)/tests/flood/service/flood \
                     $(top_builddir)/tests/flood/ebin/cloudi_service_flood.beam \
                     $(top_builddir)/tests/flood/service/jar/cloudi.jar \
                     $(top_builddir)/tests/flood/service/jar/flood.jar
	$(MKDIR_P) "$(DESTDIR)$(cloudi_prefix)/tests/flood/ebin"
	$(MKDIR_P) "$(DESTDIR)$(cloudi_prefix)/tests/flood/service"
	$(MKDIR_P) "$(DESTDIR)$(cloudi_prefix)/tests/flood/service/jar"
	$(INSTALL_DATA) "$(top_builddir)/tests/flood/ebin/cloudi_service_flood.beam" \
                    "$(DESTDIR)$(cloudi_prefix)/tests/flood/ebin/"
	$(LIBTOOL) --mode=install $(INSTALL_PROGRAM) \
             "$(top_builddir)/tests/flood/service/flood" \
             "$(DESTDIR)$(cloudi_prefix)/tests/flood/service/"
	$(INSTALL_DATA) "$(top_builddir)/tests/flood/service/jar/cloudi.jar" \
                    "$(DESTDIR)$(cloudi_prefix)/tests/flood/service/jar/"
	$(INSTALL_DATA) "$(top_builddir)/tests/flood/service/jar/flood.jar" \
                    "$(DESTDIR)$(cloudi_prefix)/tests/flood/service/jar/"
	$(INSTALL_SCRIPT) "$(top_srcdir)/tests/flood/service/flood.py" \
                      "$(DESTDIR)$(cloudi_prefix)/tests/flood/service/"
	$(INSTALL_DATA) "$(top_srcdir)/tests/flood/service/flood.rb" \
                    "$(DESTDIR)$(cloudi_prefix)/tests/flood/service/"

tests-hexpi-install: $(top_builddir)/tests/hexpi/ebin/cloudi_service_hexpi.beam \
                     $(top_builddir)/tests/hexpi/cxx_src/hexpi
	$(MKDIR_P) "$(DESTDIR)$(cloudi_prefix)/tests/hexpi/ebin"
	$(MKDIR_P) "$(DESTDIR)$(cloudi_prefix)/tests/hexpi/priv"
	$(INSTALL_DATA) "$(top_builddir)/tests/hexpi/ebin/cloudi_service_hexpi.beam" \
                    "$(DESTDIR)$(cloudi_prefix)/tests/hexpi/ebin/"
	$(LIBTOOL) --mode=install $(INSTALL_PROGRAM) \
             "$(top_builddir)/tests/hexpi/cxx_src/hexpi" \
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
        $(top_builddir)/tests/http_req/ebin/cloudi_service_http_req.beam
	$(MKDIR_P) "$(DESTDIR)$(cloudi_prefix)/tests/http_req/priv"
	$(MKDIR_P) "$(DESTDIR)$(cloudi_prefix)/tests/http_req/jar"
	$(MKDIR_P) "$(DESTDIR)$(cloudi_prefix)/tests/http_req/public_html"
	$(MKDIR_P) "$(DESTDIR)$(cloudi_prefix)/tests/http_req/ebin"
	$(LIBTOOL) --mode=install $(INSTALL_PROGRAM) \
             "$(top_builddir)/tests/http_req/c_src/http_req" \
             "$(DESTDIR)$(cloudi_prefix)/tests/http_req/priv/"
	$(INSTALL_SCRIPT) "$(top_srcdir)/tests/http_req/http_req.py" \
                    "$(DESTDIR)$(cloudi_prefix)/tests/http_req/"
	$(INSTALL_SCRIPT) "$(top_srcdir)/tests/http_req/http_req_c.py" \
                    "$(DESTDIR)$(cloudi_prefix)/tests/http_req/"
	$(INSTALL_SCRIPT) "$(top_srcdir)/tests/http_req/http_req.rb" \
                    "$(DESTDIR)$(cloudi_prefix)/tests/http_req/"
	$(INSTALL_DATA) "$(top_builddir)/tests/http_req/jar/cloudi.jar" \
                  "$(DESTDIR)$(cloudi_prefix)/tests/http_req/jar/"
	$(INSTALL_DATA) "$(top_builddir)/tests/http_req/jar/http_req.jar" \
                  "$(DESTDIR)$(cloudi_prefix)/tests/http_req/jar/"
	$(INSTALL_DATA) \
        "$(top_builddir)/tests/http_req/ebin/cloudi_service_http_req.beam" \
        "$(DESTDIR)$(cloudi_prefix)/tests/http_req/ebin/"
	$(INSTALL_DATA) "$(top_srcdir)/tests/http_req/public_html/index.html" \
                  "$(DESTDIR)$(cloudi_prefix)/tests/http_req/public_html/"
	$(INSTALL_DATA) "$(top_srcdir)/tests/http_req/public_html/style.css" \
                  "$(DESTDIR)$(cloudi_prefix)/tests/http_req/public_html/"
	$(INSTALL_DATA) \
        "$(top_srcdir)/tests/http_req/public_html/800px-Juliadim2.png" \
        "$(DESTDIR)$(cloudi_prefix)/tests/http_req/public_html/"
	$(INSTALL_DATA) "$(top_srcdir)/tests/http_req/public_html/websockets.html" \
                  "$(DESTDIR)$(cloudi_prefix)/tests/http_req/public_html/"

tests-service_api-install:
	$(MKDIR_P) "$(DESTDIR)$(cloudi_prefix)/tests/service_api"
	$(INSTALL_SCRIPT) "$(top_srcdir)/tests/service_api/run.py" \
                      "$(DESTDIR)$(cloudi_prefix)/tests/service_api/"
	$(INSTALL_SCRIPT) "$(top_srcdir)/tests/service_api/logging_on.py" \
                      "$(DESTDIR)$(cloudi_prefix)/tests/service_api/"
	$(INSTALL_SCRIPT) "$(top_srcdir)/tests/service_api/logging_off.py" \
                      "$(DESTDIR)$(cloudi_prefix)/tests/service_api/"
	$(INSTALL_SCRIPT) "$(top_srcdir)/tests/service_api/path.py" \
                      "$(DESTDIR)$(cloudi_prefix)/tests/service_api/"

tests-messaging-install: \
        $(top_builddir)/tests/messaging/ebin/cloudi_service_messaging_sequence1.beam \
        $(top_builddir)/tests/messaging/ebin/cloudi_service_messaging_sequence2.beam \
        $(top_builddir)/tests/messaging/ebin/cloudi_service_messaging_sequence3.beam \
        $(top_builddir)/tests/messaging/jar/cloudi.jar \
        $(top_builddir)/tests/messaging/jar/messaging.jar \
        $(top_builddir)/tests/messaging/cxx_src/messaging
	$(MKDIR_P) "$(DESTDIR)$(cloudi_prefix)/tests/messaging/ebin"
	$(MKDIR_P) "$(DESTDIR)$(cloudi_prefix)/tests/messaging/jar"
	$(MKDIR_P) "$(DESTDIR)$(cloudi_prefix)/tests/messaging/priv"
	$(INSTALL_DATA) \
        "$(top_builddir)/tests/messaging/ebin/cloudi_service_messaging_sequence1.beam" \
        "$(DESTDIR)$(cloudi_prefix)/tests/messaging/ebin/"
	$(INSTALL_DATA) \
        "$(top_builddir)/tests/messaging/ebin/cloudi_service_messaging_sequence2.beam" \
        "$(DESTDIR)$(cloudi_prefix)/tests/messaging/ebin/"
	$(INSTALL_DATA) \
        "$(top_builddir)/tests/messaging/ebin/cloudi_service_messaging_sequence3.beam" \
        "$(DESTDIR)$(cloudi_prefix)/tests/messaging/ebin/"
	$(INSTALL_DATA) "$(top_builddir)/tests/messaging/jar/cloudi.jar" \
                    "$(DESTDIR)$(cloudi_prefix)/tests/messaging/jar/"
	$(INSTALL_DATA) "$(top_builddir)/tests/messaging/jar/messaging.jar" \
                    "$(DESTDIR)$(cloudi_prefix)/tests/messaging/jar/"
	$(INSTALL_SCRIPT) "$(top_srcdir)/tests/messaging/messaging.py" \
                      "$(DESTDIR)$(cloudi_prefix)/tests/messaging/"
	$(INSTALL_SCRIPT) "$(top_srcdir)/tests/messaging/messaging_c.py" \
                      "$(DESTDIR)$(cloudi_prefix)/tests/messaging/"
	$(INSTALL_SCRIPT) "$(top_srcdir)/tests/messaging/messaging.rb" \
                      "$(DESTDIR)$(cloudi_prefix)/tests/messaging/"
	$(LIBTOOL) --mode=install $(INSTALL_PROGRAM) \
             "$(top_builddir)/tests/messaging/cxx_src/messaging" \
             "$(DESTDIR)$(cloudi_prefix)/tests/messaging/priv/"

tests-msg_size-install: \
        $(top_builddir)/tests/msg_size/ebin/cloudi_service_msg_size.beam \
        $(top_builddir)/tests/msg_size/jar/cloudi.jar \
        $(top_builddir)/tests/msg_size/jar/msg_size.jar \
        $(top_builddir)/tests/msg_size/cxx_src/msg_size
	$(MKDIR_P) "$(DESTDIR)$(cloudi_prefix)/tests/msg_size/ebin"
	$(MKDIR_P) "$(DESTDIR)$(cloudi_prefix)/tests/msg_size/jar"
	$(MKDIR_P) "$(DESTDIR)$(cloudi_prefix)/tests/msg_size/priv"
	$(INSTALL_DATA) \
        "$(top_builddir)/tests/msg_size/ebin/cloudi_service_msg_size.beam" \
        "$(DESTDIR)$(cloudi_prefix)/tests/msg_size/ebin/"
	$(INSTALL_DATA) "$(top_builddir)/tests/msg_size/jar/cloudi.jar" \
                    "$(DESTDIR)$(cloudi_prefix)/tests/msg_size/jar/"
	$(INSTALL_DATA) "$(top_builddir)/tests/msg_size/jar/msg_size.jar" \
                    "$(DESTDIR)$(cloudi_prefix)/tests/msg_size/jar/"
	$(INSTALL_SCRIPT) "$(top_srcdir)/tests/msg_size/msg_size.py" \
                      "$(DESTDIR)$(cloudi_prefix)/tests/msg_size/"
	$(INSTALL_SCRIPT) "$(top_srcdir)/tests/msg_size/msg_size_c.py" \
                      "$(DESTDIR)$(cloudi_prefix)/tests/msg_size/"
	$(INSTALL_SCRIPT) "$(top_srcdir)/tests/msg_size/msg_size.rb" \
                      "$(DESTDIR)$(cloudi_prefix)/tests/msg_size/"
	$(LIBTOOL) --mode=install $(INSTALL_PROGRAM) \
             "$(top_builddir)/tests/msg_size/cxx_src/msg_size" \
             "$(DESTDIR)$(cloudi_prefix)/tests/msg_size/priv/"

tests-websockets-install:
	$(MKDIR_P) "$(DESTDIR)$(cloudi_prefix)/tests/websockets"
	$(INSTALL_SCRIPT) "$(top_srcdir)/tests/websockets/websockets.py" \
                      "$(DESTDIR)$(cloudi_prefix)/tests/websockets/"

tests-zeromq-install:
	$(MKDIR_P) "$(DESTDIR)$(cloudi_prefix)/tests/zeromq"
	$(INSTALL_SCRIPT) "$(top_srcdir)/tests/zeromq/run.py" \
                      "$(DESTDIR)$(cloudi_prefix)/tests/zeromq/"

api-install:
	$(MKDIR_P) "$(DESTDIR)$(cloudi_prefix)/api"

api-erlang-install:
	$(MKDIR_P) "$(DESTDIR)$(cloudi_prefix)/api/erlang"
	$(INSTALL_DATA) "$(top_srcdir)/api/erlang/README.markdown" \
                  "$(DESTDIR)$(cloudi_prefix)/api/erlang/"

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
	$(INSTALL_DATA) "$(top_srcdir)/api/python/cloudi_c.py" \
                    "$(DESTDIR)$(cloudi_prefix)/api/python/"

api-ruby-install:
	$(MKDIR_P) "$(DESTDIR)$(cloudi_prefix)/api/ruby"
	$(INSTALL_DATA) "$(top_srcdir)/api/ruby/cloudi.rb" \
                    "$(DESTDIR)$(cloudi_prefix)/api/ruby/"
	$(INSTALL_DATA) "$(top_srcdir)/api/ruby/erlang.rb" \
                    "$(DESTDIR)$(cloudi_prefix)/api/ruby/"

service_api-install:
	$(MKDIR_P) "$(DESTDIR)$(cloudi_prefix)/service_api"

service_api-python-install:
	$(MKDIR_P) "$(DESTDIR)$(cloudi_prefix)/service_api/python/jsonrpclib/jsonrpclib"
	$(INSTALL_DATA) "$(top_srcdir)/service_api/python/cloudi_service_api.py" \
                    "$(DESTDIR)$(cloudi_prefix)/service_api/python/"
	$(INSTALL_DATA) "$(top_srcdir)/service_api/python/jsonrpclib/LICENSE.txt" \
                    "$(DESTDIR)$(cloudi_prefix)/service_api/python/jsonrpclib/"
	$(INSTALL_DATA) "$(top_srcdir)/service_api/python/jsonrpclib/README.md" \
                    "$(DESTDIR)$(cloudi_prefix)/service_api/python/jsonrpclib/"
	$(INSTALL_DATA) \
        "$(top_srcdir)/service_api/python/jsonrpclib/jsonrpclib/__init__.py" \
        "$(DESTDIR)$(cloudi_prefix)/service_api/python/jsonrpclib/jsonrpclib/"
	$(INSTALL_DATA) \
        "$(top_srcdir)/service_api/python/jsonrpclib/jsonrpclib/jsonrpc.py" \
        "$(DESTDIR)$(cloudi_prefix)/service_api/python/jsonrpclib/jsonrpclib/"
	$(INSTALL_DATA) \
        "$(top_srcdir)/service_api/python/jsonrpclib/jsonrpclib/config.py" \
        "$(DESTDIR)$(cloudi_prefix)/service_api/python/jsonrpclib/jsonrpclib/"
	$(INSTALL_DATA) \
        "$(top_srcdir)/service_api/python/jsonrpclib/jsonrpclib/history.py" \
        "$(DESTDIR)$(cloudi_prefix)/service_api/python/jsonrpclib/jsonrpclib/"
	$(INSTALL_DATA) \
        "$(top_srcdir)/service_api/python/jsonrpclib/jsonrpclib/jsonclass.py" \
        "$(DESTDIR)$(cloudi_prefix)/service_api/python/jsonrpclib/jsonrpclib/"

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

