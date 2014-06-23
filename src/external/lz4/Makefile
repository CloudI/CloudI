.PHONY: doc

all:
	./rebar compile
	./rebar doc
	./rebar xref
	./rebar eunit

compile:
	./rebar compile

doc:
	./rebar doc

xref: compile
	./rebar xref

clean:
	./rebar clean

test: xref
	./rebar eunit

