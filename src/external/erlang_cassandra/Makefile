APPLICATION := erlang_cassandra

ERL := erl
EPATH := -pa ebin -pz deps/*/ebin
TEST_EPATH := -pa .eunit -pz deps/*/ebin
PLT = .erlang_cassandra_plt
ERL_LIB_DIR := $(shell if [ -d /usr/lib/erlang/lib ] ; then echo /usr/lib/erlang/lib ; else echo /usr/local/lib/erlang/lib ; fi)
PLT_APPS := $(shell ls $(ERL_LIB_DIR) | grep -v interface | sed -e 's/-[0-9.]*//')

.PHONY: all build-plt compile console deps doc clean depclean distclean dialyze release telstart test test-console

all: compile

compile:
	@rebar compile

deps:
	@rebar get-deps

doc:
	@rebar skip_deps=true doc

clean:
	@rebar skip_deps=true clean

depclean:
	@rebar clean

distclean:
	@rebar delete-deps

build-plt:
	@dialyzer --build_plt --apps kernel stdlib sasl crypto ssl inets tools xmerl runtime_tools compiler syntax_tools mnesia public_key

dialyze: compile
	@dialyzer -r ebin -r deps/proper -r deps/thrift -r deps/poolboy -r deps/reltool_util \
		-Wno_undefined_callbacks

test: compile
	@rebar skip_deps=true ct verbose=1

console:
	$(ERL) -sname $(APPLICATION) $(EPATH) -config app

test-console: test
	$(ERL) -sname $(APPLICATION)_test $(TEST_EPATH) -config app
