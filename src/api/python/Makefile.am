instdir=$(DESTDIR)$(cloudi_prefix)/api/python/
inst_LTLIBRARIES = libcloudi_py.la

libcloudi_py_la_SOURCES = cloudi_py.c
libcloudi_py_la_CFLAGS = -DNDEBUG -g -Wall \
                         -I$(top_srcdir)/api/c/ \
                         $(PYTHON_CFLAGS)
libcloudi_py_la_LDFLAGS = -L$(top_builddir)/api/c/ \
                          $(PYTHON_LDFLAGS)
libcloudi_py_la_LIBADD = -lcloudi

