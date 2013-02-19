LINUX=$(shell uname | grep Linux | wc -l | xargs echo)
DEPS=../deps

ifeq ($(LINUX),1)
ZMQ_FLAGS=--with-pic
else
ZMQ_FLAGS=
endif

ifndef ZEROMQ_VERSION
ZEROMQ_VERSION=v3.1.0
endif

all: $(DEPS)/zeromq3/src/.libs/libzmq.a

clean:
	if test -e $(DEPS)/zeromq3/Makefile; then \
		cd $(DEPS)/zeromq3; make clean; \
	else \
		true; \
	fi

distclean:
	@rm -rf $(DEPS)

$(DEPS):
	@mkdir -p $(DEPS)

$(DEPS)/zeromq3: $(DEPS)
	@git clone https://github.com/zeromq/libzmq.git $(DEPS)/zeromq3
	@echo $(ZEROMQ_VERSION)
	@cd $(DEPS)/zeromq3 && git checkout $(ZEROMQ_VERSION)

$(DEPS)/zeromq3/src/.libs/libzmq.a: $(DEPS)/zeromq3
	@cd $(DEPS)/zeromq3 && ./autogen.sh && ./configure $(ZMQ_FLAGS) && make
