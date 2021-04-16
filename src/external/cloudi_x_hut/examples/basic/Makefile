PROJECT 			= hut_example
PROJECT_DESCRIPTION = example of using various hut configurations
PROJECT_VERSION 	= 0.1.0

DEPS = lager

all:: run_example_default

include ../../erlang.mk

hut_dir 			= $(shell cd ../../ ; pwd)
exec_run 			= hut_example
exec 				= erl -pz $(hut_dir)/ebin -pz ebin $(exec_opts) -run $(exec_run) -run init stop -noshell
erlc_includes		= -pz $(hut_dir)

.PHONY: run_example_default
run_example_default: export ERLC_OPTS = $(erlc_includes)
run_example_default: clean app
	$(exec)

.PHONY: run_example_sasl
run_example_sasl: export ERLC_OPTS = -DHUT_SASL $(erlc_includes)
run_example_sasl: clean app
	$(exec)

.PHONY: run_example_default_no_gate
run_example_default_no_gate: export ERLC_OPTS = $(erlc_includes)
run_example_default_no_gate: exec_opts = -hut use_log_level_gate false
run_example_default_no_gate: clean app
	$(exec)

.PHONY: run_example_default_debug_enabled
run_example_default_debug_enabled: export ERLC_OPTS = $(erlc_includes)
run_example_default_debug_enabled: exec_opts = -hut level debug
run_example_default_debug_enabled: clean app
	$(exec)

.PHONY: run_example_default_warning_enabled
run_example_default_warning_enabled: export ERLC_OPTS = $(erlc_includes)
run_example_default_warning_enabled: exec_opts = -hut level warning
run_example_default_warning_enabled: clean app
	$(exec)

.PHONY: run_example_ioformat
run_example_ioformat: export ERLC_OPTS = -DHUT_IOFORMAT $(erlc_includes)
run_example_ioformat: clean app
	$(exec)

.PHONY: run_example_custom
run_example_custom: export ERLC_OPTS = -DHUT_CUSTOM -DHUT_CUSTOM_CB=$(PROJECT) $(erlc_includes)
run_example_custom: clean app
	$(exec)

.PHONY: run_example_noop
run_example_noop: export ERLC_OPTS = -DHUT_NOOP $(erlc_includes)
run_example_noop: clean app
	$(exec)

.PHONY: run_example_lager
run_example_lager: export ERLC_OPTS = -DHUT_LAGER +'{parse_transform, lager_transform}' $(erlc_includes)
run_example_lager: exec_opts = -run lager start
run_example_lager: clean app
	$(exec)

.PHONY: run_example_lager_custom_sink
run_example_lager_custom_sink: export ERLC_OPTS = -DHUT_LAGER -DHUT_LAGER_SINK=mysink +'{parse_transform, lager_transform}' +"{lager_extra_sinks, [mysink]}" $(erlc_includes)
run_example_lager_custom_sink: exec_opts = -config app -run lager start
run_example_lager_custom_sink: clean app
	$(exec)

.PHONY: compile_example_elixir
compile_example_elixir: export ERLC_OPTS = $(erlc_includes)
compile_example_elixir: clean app
