LINUX=$(shell uname | grep -E "(Linux|NetBSD)" | wc -l | xargs echo)
DEPS=../deps

ifeq ($(LINUX),1)
ZMQ_FLAGS=--with-pic
else
ZMQ_FLAGS=
endif

ifndef ZEROMQ_VERSION
ZEROMQ_VERSION=v2.1.11
endif

all: $(DEPS)/zeromq2/src/.libs/libzmq.a

clean:
	if test -e $(DEPS)/zeromq2/Makefile; then \
		cd $(DEPS)/zeromq2; make clean; \
	else \
		true; \
	fi

distclean:
	@rm -rf $(DEPS)

$(DEPS)/zeromq2:
	@mkdir $(DEPS)
	@git clone git://github.com/zeromq/zeromq2-x.git $(DEPS)/zeromq2
	@echo $(ZEROMQ_VERSION)
	@cd $(DEPS)/zeromq2 && git checkout $(ZEROMQ_VERSION)

$(DEPS)/zeromq2/src/.libs/libzmq.a: $(DEPS)/zeromq2
	@cd $(DEPS)/zeromq2 && ./autogen.sh && ./configure $(ZMQ_FLAGS) && make
