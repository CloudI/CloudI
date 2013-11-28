instdir=$(DESTDIR)$(cloudi_prefix)/api/c/
inst_LTLIBRARIES = libcloudi.la
nodist_inst_HEADERS = cloudi.h cloudi.hpp

libcloudi_la_SOURCES = \
    cloudi.cpp \
    assert.cpp \
    timer.cpp
libcloudi_la_CPPFLAGS = -I$(ERLANG_LIB_DIR_erl_interface)/include/ \
                        $(BOOST_CPPFLAGS) \
                        $(BACKTRACE_CPPFLAGS) \
                        $(CXXFLAGS)
libcloudi_la_LDFLAGS = -L$(ERLANG_LIB_DIR_erl_interface)/lib/ \
                       -no-undefined -export-dynamic \
                       $(BOOST_LDFLAGS) \
                       $(BACKTRACE_LDFLAGS)
libcloudi_la_LIBADD = -lei $(RT_LIB) $(BACKTRACE_LIB)

