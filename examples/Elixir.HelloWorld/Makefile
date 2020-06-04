#-*-Mode:make;coding:utf-8;tab-width:4;c-basic-offset:4-*-
# ex: set ft=make fenc=utf-8 sts=4 ts=4 sw=4 noet nomod:

.PHONY : all clean release

CLOUDI_VERSION=2.0.0
CLOUDI_PATH=/usr/local/lib/cloudi-$(CLOUDI_VERSION)/lib/cloudi_core-$(CLOUDI_VERSION)

all: lib/HelloWorld.ex
	elixir -pz $(CLOUDI_PATH)/ebin -S mix compile

clean:
	mix clean
	rm -rf release

release: all reltool.config
	../../src/lib/reltool_util/release

