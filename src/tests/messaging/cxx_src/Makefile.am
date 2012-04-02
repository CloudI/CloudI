noinst_PROGRAMS = messaging
messaging_SOURCES = assert.cpp main.cpp timer.cpp
messaging_CPPFLAGS = -I$(top_srcdir)/api/c/ $(BOOST_CPPFLAGS)
messaging_LDFLAGS = -L$(top_builddir)/api/c/ $(BOOST_LDFLAGS)
messaging_LDADD = -lcloudi $(BOOST_THREAD_LIB)
if HAVE_CLOCK_GETTIME_RT
messaging_LDADD += -lrt
endif

