instdir=$(DESTDIR)$(cloudi_prefix)/api/python/
inst_LTLIBRARIES = libcloudi_py.la

libcloudi_py_la_SOURCES = cloudi_py.cpp
libcloudi_py_la_CPPFLAGS = -g -Wall \
                           $(PYTHON_CFLAGS) -O0 \
                           -I$(top_srcdir)/api/c/ \
                           $(CXXFLAGS)
libcloudi_py_la_LDFLAGS = $(PYTHON_LDFLAGS) -shared \
                          -no-undefined -module -avoid-version \
                          -export-symbols-regex initlibcloudi_py \
                          -weak libcloudi.la \
                          -L$(ERLANG_LIB_DIR_erl_interface)/lib/ 
# libtool requires linking in the object files, since the library
# is a local dependency that is not yet installed, but is a dynamic library
libcloudi_py_la_LIBADD = ../c/libcloudi_la-cloudi.lo \
                         ../c/libcloudi_la-assert.lo \
                         ../c/libcloudi_la-timer.lo \
                         -lei $(RT_LIB) $(BACKTRACE_LIB) -lstdc++

