.PHONY : all clean release

CLOUDI_VERSION=1.5.4
CLOUDI_PATH=/usr/local/lib/cloudi-$(CLOUDI_VERSION)/lib/cloudi_core-$(CLOUDI_VERSION)

all: lib/HelloWorld1.ex
	elixir -pz $(CLOUDI_PATH)/ebin -S mix compile

clean:
	mix clean
	rm -rf release

release: all reltool.config
	../../src/lib/reltool_util/release

