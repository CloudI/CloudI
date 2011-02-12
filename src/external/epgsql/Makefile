NAME		:= epgsql
VERSION		:= 1.3

ERL  		:= erl
ERLC 		:= erlc

# ------------------------------------------------------------------------

ERLC_FLAGS	:= -Wall -I include

SRC			:= $(wildcard src/*.erl)
TESTS 		:= $(wildcard test_src/*.erl)
RELEASE		:= $(NAME)-$(VERSION).tar.gz

APPDIR		:= $(NAME)-$(VERSION)
BEAMS		:= $(SRC:src/%.erl=ebin/%.beam) 

compile: $(BEAMS) ebin/$(NAME).app

app: compile
	@mkdir -p $(APPDIR)/ebin
	@cp -r ebin/* $(APPDIR)/ebin/
	@cp -r include $(APPDIR)

release: app
	@tar czvf $(RELEASE) $(APPDIR)

clean:
	@rm -f ebin/*.{beam,app}
	@rm -rf $(NAME)-$(VERSION) $(NAME)-*.tar.gz

test: $(TESTS:test_src/%.erl=test_ebin/%.beam) compile
	@dialyzer -n --src -c src
	$(ERL) -pa ebin/ -pa test_ebin/ -noshell -s pgsql_tests run_tests -s init stop

# ------------------------------------------------------------------------

.SUFFIXES: .erl .beam
.PHONY:    app compile clean test

ebin/%.beam : src/%.erl
	$(ERLC) $(ERLC_FLAGS) -o $(dir $@) $<

ebin/%.app : src/%.app.src
	sed -e s/VERSION/$(VERSION)/g $< > $@

test_ebin/%.beam : test_src/%.erl
	$(ERLC) $(ERLC_FLAGS) -o $(dir $@) $<
