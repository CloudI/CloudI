#-*-Mode:make;coding:utf-8;tab-width:4;c-basic-offset:4-*-
# ex: set ft=make fenc=utf-8 sts=4 ts=4 sw=4 noet nomod:

.PHONY : all clean release

CLOUDI_VERSION=1.6.0
CLOUDI_PATH=/usr/local/lib/cloudi-$(CLOUDI_VERSION)/lib/cloudi_core-$(CLOUDI_VERSION)

all: src/hello_world4.erl
	erlc -pz $(CLOUDI_PATH) -pz $(CLOUDI_PATH)/ebin -o ebin src/hello_world4.erl

clean:
	rm -f ebin/hello_world4.beam
	rm -rf release

release: all reltool.config
	../../src/lib/reltool_util/release

