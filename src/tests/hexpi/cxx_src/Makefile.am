bin_PROGRAMS = hexpi
hexpi_SOURCES = assert.cpp main.cpp timer.cpp \
                piqpr8_gmp.cpp piqpr8_gmp_verify.cpp
hexpi_CPPFLAGS = -I$(top_builddir)/api/c/
hexpi_LDFLAGS = -L$(top_builddir)/api/c/
hexpi_LDADD = -lcloudi -lboost_thread
if HAVE_CLOCK_GETTIME_RT
hexpi_LDADD += -lrt
endif

