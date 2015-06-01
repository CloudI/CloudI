#-*-Mode:make;coding:utf-8;tab-width:4;c-basic-offset:4-*-
# ex: set ft=make fenc=utf-8 sts=4 ts=4 sw=4 noet:

instdir = "$(cloudi_prefix)/tests/hexpi/ebin"
directinstdir = "$(DESTDIR)$(instdir)"
beamdir = "$(instdir)"
buildbeamdir = ../ebin
APPLICATION = cloudi_service_hexpi.app
beam_DATA = cloudi_service_hexpi.beam
CLEANFILES = $(beam_DATA) \
             $(buildbeamdir)/$(APPLICATION) \
             $(buildbeamdir)/cloudi_service_hexpi.beam

all-local: $(beam_DATA) $(srcdir)/$(APPLICATION)
	$(MKDIR_P) $(buildbeamdir) || exit 0
	cp -f $(srcdir)/$(APPLICATION) $(buildbeamdir)
	cp -f $(beam_DATA) $(buildbeamdir)

.erl.beam:
	@ERLC@ -b beam \
           -pz $(top_builddir)/lib/cloudi_core/ebin \
           -pz $(top_builddir)/lib/cloudi_service_map_reduce/ebin \
           $(ERLC_OPTS) -o $@ $<

install-exec-hook: $(buildbeamdir)/$(APPLICATION) \
                   $(buildbeamdir)/cloudi_service_hexpi.beam
	$(MKDIR_P) $(directinstdir)
	$(INSTALL_DATA) $(buildbeamdir)/$(APPLICATION) $(directinstdir)
	$(INSTALL_DATA) $(buildbeamdir)/cloudi_service_hexpi.beam $(directinstdir)

