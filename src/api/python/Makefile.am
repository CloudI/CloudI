#-*-Mode:make;coding:utf-8;tab-width:4;c-basic-offset:4-*-
# ex: set ft=make fenc=utf-8 sts=4 ts=4 sw=4 noet:

.PHONY: \
    python-install \
    python-c-install

instdir = "$(DESTDIR)$(cloudi_prefix)/api/python"

python-install:
	$(MKDIR_P) $(instdir)
	$(INSTALL_DATA) $(srcdir)/erlang.py $(instdir)
	$(INSTALL_DATA) $(srcdir)/cloudi.py $(instdir)

if PYTHON_C_SUPPORT
inst_LTLIBRARIES = libcloudi_py.la

libcloudi_py_la_SOURCES = cloudi_py.cpp
libcloudi_py_la_CPPFLAGS = -g -Wall \
                           $(PYTHON_CFLAGS) -O0 \
                           -I$(top_srcdir)/api/c/ \
                           $(CXXFLAGS)
libcloudi_py_la_LDFLAGS = $(PYTHON_LDFLAGS) -shared \
                          -no-undefined -module -avoid-version \
                          -export-symbols-regex '.*libcloudi_py' \
                          -weak libcloudi.la \
                          -L$(ERLANG_LIB_DIR_erl_interface)/lib/ 
# libtool requires linking in the object files, since the library
# is a local dependency that is not yet installed, but is a dynamic library
libcloudi_py_la_LIBADD = ../c/libcloudi_la-cloudi.lo \
                         ../c/libcloudi_la-assert.lo \
                         ../c/libcloudi_la-timer.lo \
                         -lei $(RT_LIB) $(BACKTRACE_LIB) -lstdc++

PYTHON_C_INSTALL_HOOK = python-c-install
endif

python-c-install: python-install
	$(INSTALL_DATA) $(srcdir)/cloudi_c.py $(instdir)

install-exec-hook: python-install $(PYTHON_C_INSTALL_HOOK)

