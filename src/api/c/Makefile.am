#-*-Mode:make;coding:utf-8;tab-width:4;c-basic-offset:4-*-
# ex: set ft=make fenc=utf-8 sts=4 ts=4 sw=4 noet nomod:

instdir = "$(cloudi_prefix)/api/c"
inst_LTLIBRARIES = libcloudi.la
if CXX_SUPPORT
    CXX_SUPPORT_HEADER = cloudi.hpp
endif
nodist_inst_HEADERS = cloudi.h $(CXX_SUPPORT_HEADER)

libcloudi_la_SOURCES = \
    cloudi.cpp \
    assert.cpp \
    timer.cpp \
    bind.cpp
libcloudi_la_CPPFLAGS = -I$(ERLANG_LIB_DIR_erl_interface)/include/ \
                        $(BOOST_CPPFLAGS) \
                        $(BACKTRACE_CXXFLAGS) \
                        $(BIND_CXXFLAGS)
libcloudi_la_LDFLAGS = -L$(ERLANG_LIB_DIR_erl_interface)/lib/ \
                       -no-undefined \
                       $(BOOST_LDFLAGS) \
                       $(BACKTRACE_LDFLAGS)
libcloudi_la_LIBADD = -lei $(BIND_LIBS) $(RT_LIB) $(BACKTRACE_LIB) $(LIBM)

