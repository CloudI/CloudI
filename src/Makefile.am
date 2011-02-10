ACLOCAL_AMFLAGS = -I m4
SUBDIRS = lib

all-local: rebar 
	./rebar compile

run: all
	$(ERLANG_ROOT_DIR)/bin/erl \
      `find lib/ -name "ebin" -o -name "priv" | sed 's/^/-pz /'`

#./rebar generate
#./rebar console

