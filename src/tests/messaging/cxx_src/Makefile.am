noinst_PROGRAMS = messaging
messaging_SOURCES = assert.cpp main.cpp timer.cpp
messaging_CPPFLAGS = -I$(top_srcdir)/api/c/ $(BOOST_CPPFLAGS)
messaging_LDFLAGS = $(BOOST_LDFLAGS)
messaging_LDADD = $(top_builddir)/api/c/libcloudi.la \
                  $(BOOST_THREAD_LIB) $(BOOST_SYSTEM_LIB) \
                  $(RT_LIB)

