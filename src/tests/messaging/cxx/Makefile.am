#-*-Mode:make;coding:utf-8;tab-width:4;c-basic-offset:4-*-
# ex: set ft=make fenc=utf-8 sts=4 ts=4 sw=4 noet nomod:

instdir = "$(cloudi_prefix)/tests/messaging"
inst_PROGRAMS = messaging_cxx
messaging_cxx_SOURCES = assert.cpp main.cpp timer.cpp
messaging_cxx_CPPFLAGS = -I$(top_srcdir)/api/c/ $(BOOST_CPPFLAGS)
messaging_cxx_LDFLAGS = $(BOOST_LDFLAGS)
messaging_cxx_LDADD = $(top_builddir)/api/c/libcloudi.la \
                      $(BOOST_THREAD_LIB) $(BOOST_SYSTEM_LIB) \
                      $(RT_LIB)

