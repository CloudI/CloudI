.PHONY: compile xref eunit clean doc check make deps test

REBAR=./rebar

all: compile

# for busy typos
m: all
ma: all
mak: all
make: all

console: compile
	@erl -pa ebin
deps:
	@$(REBAR) update-deps get-deps

compile:
	@$(REBAR) compile

xref: compile
	@$(REBAR) xref

test: compile xref
	@./rebar eunit

clean:
	@$(REBAR) clean

doc:
	@$(REBAR) doc

APPS = kernel stdlib runtime_tools
COMBO_PLT = $(HOME)/.msgpack_dialyzer_plt

check_plt: xref
	dialyzer --check_plt --plt $(COMBO_PLT) --apps $(APPS)

build_plt: xref
	dialyzer --build_plt --output_plt $(COMBO_PLT) --apps $(APPS)

dialyzer: xref
	@echo
	@echo Use "'make check_plt'" to check PLT prior to using this target.
	@echo Use "'make build_plt'" to build PLT prior to using this target.
	@echo
	@sleep 1
	dialyzer -Wno_return --plt $(COMBO_PLT) ebin | fgrep -v -f ./dialyzer.ignore-warnings



check: compile xref
#	@echo "you need $(REBAR) build-plt before make check"
#	@$(REBAR) build-plt
	dialyzer --check
#	@$(REBAR) check-plt
#	@$(REBAR) dialyze

crosslang:
	@echo "do ERL_LIBS=../ before you make crosslang or fail"
	cd test && make crosslang
