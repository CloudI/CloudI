REBAR ?= ./rebar3
REBAR_CONFIG ?= ./rebar.config

all:: run_example_default

hut_dir 			= $(shell cd ../../ ; pwd)
exec_run 			= hut_example_rebar3
exec 				= erl -pz ./_build/default/lib/*/ebin $(exec_opts) -run $(exec_run) -run init stop -noshell

$(REBAR):
	curl -L https://s3.amazonaws.com/rebar3/rebar3 > $@
	chmod +x $@

.PHONY: run_example_default
run_example_default: $(REBAR)
	cp rebar.config.default $(REBAR_CONFIG)
	@$(MAKE) reset
	$(exec)
	@rm $(REBAR_CONFIG)

.PHONY: run_example_sasl
run_example_sasl: $(REBAR)
	cp rebar.config.sasl $(REBAR_CONFIG)
	@$(MAKE) reset
	$(exec)
	@rm $(REBAR_CONFIG)

.PHONY: run_example_noop
run_example_noop: $(REBAR)
	cp rebar.config.noop $(REBAR_CONFIG)
	@$(MAKE) reset
	$(exec)
	@rm $(REBAR_CONFIG)

.PHONY: run_example_lager
run_example_lager: exec_opts = -run lager start
run_example_lager: $(REBAR)
	cp rebar.config.lager $(REBAR_CONFIG)
	@$(MAKE) reset
	$(exec)
	@rm $(REBAR_CONFIG)

.PHONY: reset
reset: $(REBAR)
	rm -rf ./_build ./rebar.lock
	$(REBAR) deps
	$(REBAR) clean --all
	$(REBAR) compile
