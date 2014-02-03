.PHONY : all clean release

CLOUDI_VERSION=1.3.1
CLOUDI_PATH=/usr/local/lib/cloudi-$(CLOUDI_VERSION)/lib/cloudi_core-$(CLOUDI_VERSION)

all: src/hello_world1.erl
	erlc -pz $(CLOUDI_PATH) -pz $(CLOUDI_PATH)/ebin -o ebin src/hello_world1.erl

clean:
	rm -f ebin/hello_world1.beam
	rm -rf release

release: all reltool.config
	../../src/lib/reltool_util/release

