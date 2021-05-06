#-*-Mode:make;coding:utf-8;tab-width:4;c-basic-offset:4-*-
# ex: set ft=make fenc=utf-8 sts=4 ts=4 sw=4 noet nomod:

instdir = "$(cloudi_prefix)/api/ats/v2"
directinstdir = "$(DESTDIR)$(instdir)"

inst_LTLIBRARIES = libcloudi_ats2.la

CLEANFILES = cloudi_sats.c cloudi_dats.c
libcloudi_ats2_la_SOURCES = cloudi_sats.c cloudi_dats.c
libcloudi_ats2_la_CFLAGS = $(ATS2_CFLAGS) $(CFLAGS)
libcloudi_ats2_la_LDFLAGS = $(ATS2_LDFLAGS) -shared \
                            -no-undefined -module -avoid-version \
                            -weak libcloudi.la \
                            -L$(ERLANG_LIB_DIR_erl_interface)/lib/ 
# libtool requires linking in the object files, since the library
# is a local dependency that is not yet installed, but is a dynamic library
libcloudi_ats2_la_LIBADD = ../../c/libcloudi_la-cloudi.lo \
                           ../../c/libcloudi_la-assert.lo \
                           ../../c/libcloudi_la-timer.lo \
                           $(ATS2_LIB) \
                           -lei $(RT_LIB) $(BACKTRACE_LIB) -lstdc++

%_sats.c: %.sats
	PATSHOME=$(PATSHOME) $(ATS2CC) $(ATS2CFLAGS) -ccats $<
%_dats.c: %.dats
	PATSHOME=$(PATSHOME) $(ATS2CC) $(ATS2CFLAGS) -ccats $<

install-exec-hook:
	$(MKDIR_P) $(directinstdir)
	$(INSTALL_DATA) $(srcdir)/cloudi.hats $(directinstdir)
	$(INSTALL_DATA) $(srcdir)/cloudi.sats $(directinstdir)
	$(INSTALL_DATA) $(srcdir)/cloudi.dats $(directinstdir)

