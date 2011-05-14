noinst_LIBRARIES = libcloudi.a
noinst_LTLIBRARIES = libcloudi.la

nodist_noinst_HEADERS = cloudi.h

cloudi_libtool.cpp: cloudi.cpp
	cp -f $(srcdir)/cloudi.cpp $(srcdir)/cloudi_libtool.cpp
assert_libtool.cpp: assert.cpp
	cp -f $(srcdir)/assert.cpp $(srcdir)/assert_libtool.cpp
CLEANFILES = $(srcdir)/cloudi_libtool.cpp \
             $(srcdir)/assert_libtool.cpp

libcloudi_la_SOURCES = cloudi_libtool.cpp assert_libtool.cpp
libcloudi_la_CPPFLAGS = -I$(ERLANG_LIB_DIR_erl_interface)/include/ \
                        $(CXXFLAGS)
libcloudi_la_LDFLAGS = -L$(ERLANG_LIB_DIR_erl_interface)/lib/
libcloudi_la_LIBADD = -lei

libcloudi_a_SOURCES = cloudi.cpp assert.cpp
libcloudi_a_CPPFLAGS = -I$(ERLANG_LIB_DIR_erl_interface)/include/ \
                       $(CXXFLAGS)

