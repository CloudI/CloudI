.PHONY : all clean release

CLOUDI_VERSION=1.3.3
CLOUDI_PATH=/usr/local/lib/cloudi-$(CLOUDI_VERSION)/lib/cloudi_core-$(CLOUDI_VERSION)

all: lib/HelloWorld1.ex \
     $(CLOUDI_PATH)/include/CloudILogger.ex
	mix compile

clean:
	mix clean
	rm -rf release

release: all reltool.config
	../../src/lib/reltool_util/release

