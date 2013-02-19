all: compile

compile:
	@./rebar compile

perftest: compile
	@cd perf && erlc erlzmq_perf.erl

clean:
	@./rebar clean

distclean: clean
	@cd c_src;make distclean

test: compile
	@./rebar eunit

docs:
	@./rebar doc

bench: perftest
	@echo 'Running benchmarks, this could take some time...'
	@mkdir -p graphs
	@./perf/perfgraphs.py
	@mv -f *.png graphs/

