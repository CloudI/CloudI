ACLOCAL_AMFLAGS = -I m4
SUBDIRS = lib

all-local: rebar 
	./rebar compile

clean-local: rebar
	./rebar clean

clean-configure: config.log
	rm -f aclocal.m4 config.h config.h.in config.log config.status stamp-h1 \
          config/* rebar Makefile Makefile.in configure
	rm -rf autom4te.cache
	rm -rf lib/cloudi/cxx_src/.deps/
	find lib -name "Makefile.in" | xargs rm -f
	find lib -name "Makefile" | xargs rm -f

dialyze: rebar
	./rebar dialyze

run: all
	$(ERLANG_ROOT_DIR)/bin/erl \
      -pz external/epgsql/ebin \
      -pz external/emysql/ebin \
      -pz external/etokyotyrant/ebin \
      -pz external/ememcached/ebin \
      -pz external/ecouchdb/ebin \
      -pz external/misultin/ebin \
      `find lib/ -name "ebin" -o -name "priv" | sed 's/^/-pz /'`

#./rebar generate
#./rebar console

