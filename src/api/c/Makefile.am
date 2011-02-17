lib_LIBRARIES = libcloudi.a
lib_LTLIBRARIES = libcloudi.la

include_HEADERS = cloudi.h

cloudi_libtool.cpp: cloudi.cpp
	cp -f cloudi.cpp cloudi_libtool.cpp
assert_libtool.cpp: assert.cpp
	cp -f assert.cpp assert_libtool.cpp
CLEANFILES = cloudi_libtool.cpp assert_libtool.cpp

libcloudi_la_SOURCES = cloudi_libtool.cpp assert_libtool.cpp
libcloudi_la_CPPFLAGS = -I$(ERLANG_LIB_DIR_erl_interface)/include/
libcloudi_la_LIBADD = -lei
libcloudi_la_LDFLAGS = -L$(ERLANG_LIB_DIR_erl_interface)/lib/

libcloudi_a_SOURCES = cloudi.cpp assert.cpp
libcloudi_a_CPPFLAGS = -I$(ERLANG_LIB_DIR_erl_interface)/include/

