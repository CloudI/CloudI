#-*-Mode:make;coding:utf-8;tab-width:4;c-basic-offset:4-*-
# ex: set ft=make fenc=utf-8 sts=4 ts=4 sw=4 noet nomod:

instdir = "$(cloudi_prefix)/tests/null"
inst_PROGRAMS = null_ats2
CLEANFILES = main_dats.c
null_ats2_SOURCES = main_dats.c
null_ats2_CFLAGS = $(ATS2_CFLAGS) $(PTHREAD_CFLAGS)
null_ats2_LDADD = $(top_builddir)/api/ats/v2/libcloudi_ats2.la \
                  $(PTHREAD_LIBS) -lstdc++

%_dats.c: %.dats
	PATSHOME=$(PATSHOME) $(ATS2CC) $(ATS2CFLAGS) \
                         -IIATS $(top_srcdir)/api/ats/v2/ -ccats $<

