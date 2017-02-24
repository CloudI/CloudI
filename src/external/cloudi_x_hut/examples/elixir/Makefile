.PHONY: all clean deps run
all: clean deps run

clean:
	mix do clean, deps.clean --all

deps:
	mix deps.get

run:
	mix run -e HutExampleElixir.start
