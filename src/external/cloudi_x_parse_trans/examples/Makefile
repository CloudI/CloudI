SOURCES := $(wildcard *.erl)
HEADERS := $(wildcard *.hrl)
BEAMS := $(patsubst %.erl,%.beam,$(SOURCES))

.PHONY: all clean

all: $(BEAMS)

test: $(APPLICATION) $(TEST_BEAMS) ../util/run_test.beam
	@echo Running tests
	@erl -pa ../util/ -pa ../ebin/ -pa test/ -noinput -s run_test run

test_exprecs.beam: ../ebin/exprecs.beam test_exprecs.erl
	@echo Compiling test_exprecs.erl
	@erlc +debug_info -pa ../ebin -pa . -I ../include test_exprecs.erl

test.beam: test_pt.beam
ex_pmod.beam: pmod.beam

%.beam: %.erl $(HEADERS)
	@echo Compiling $<
	@erlc +debug_info -pa ../ebin -pa . -I ../include $<

clean:
	@echo Cleaning
	@rm -f *.beam
	@rm -f *~
	@rm -f *.xfm
	@rm -f *.xforms

