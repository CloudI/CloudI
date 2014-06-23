.PHONY: all deps compile dialyze wait test console clean clean-db

APPS = dialyzer.apps
PLT = apps.plt
DB_HOST = localhost
DB_PORT = 9042
TIMEOUT = 15

all: deps compile

deps:
	@ rebar get-deps

compile:
	@ rebar compile

dialyze: compile $(PLT)
	@ echo "==> (dialyze)"
	@ dialyzer --plt $(PLT) ebin \
	  -Wunmatched_returns \
	  -Wno_undefined_callbacks

$(PLT): dialyzer.apps
	@ echo "==> (dialyze)"
	@ printf "Building $(PLT) file..."
	@- dialyzer -q --build_plt --output_plt $(PLT) \
	   --apps $(shell cat $(APPS))
	@ echo " done"

wait:
	@ ./scripts/wait.escript $(DB_HOST) $(DB_PORT) $(TIMEOUT)

test: compile wait
	@ rebar skip_deps=true ct

console: compile
	@ erl -pa ebin deps/*/ebin

clean:
	@ rebar clean

clean-db:
	@ (cd scripts; ./clean.escript)
