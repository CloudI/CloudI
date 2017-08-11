#-*-Mode:make;coding:utf-8;tab-width:4;c-basic-offset:4-*-
# ex: set ft=make fenc=utf-8 sts=4 ts=4 sw=4 noet nomod:

instdir = "$(cloudi_prefix)/tests/hexpi"
inst_PROGRAMS = hexpi_cxx
hexpi_cxx_SOURCES = assert.cpp main.cpp timer.cpp \
                piqpr8_gmp.cpp piqpr8_gmp_verify.cpp
hexpi_cxx_CPPFLAGS = -I$(top_srcdir)/api/c/ $(BOOST_CPPFLAGS) \
                     $(GMP_H_CFLAGS)
hexpi_cxx_LDFLAGS = $(BOOST_LDFLAGS) $(GMP_LDFLAGS)
hexpi_cxx_LDADD = $(top_builddir)/api/c/libcloudi.la \
                  $(BOOST_THREAD_LIB) $(BOOST_SYSTEM_LIB) \
                  $(GMP_LIB) $(RT_LIB)

