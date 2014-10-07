#-*-Mode:make;coding:utf-8;tab-width:4;c-basic-offset:4-*-
# ex: set ft=make fenc=utf-8 sts=4 ts=4 sw=4 noet:

instdir = "$(DESTDIR)$(cloudi_prefix)/tests/messaging/ebin"
beamdir = ../ebin
APPLICATION = cloudi_service_messaging_sequence.app
beam_DATA = cloudi_service_messaging_sequence1.beam \
            cloudi_service_messaging_sequence2.beam \
            cloudi_service_messaging_sequence3.beam \
            cloudi_service_messaging_sequence4.beam
CLEANFILES = $(beam_DATA) \
             $(beamdir)/$(APPLICATION) \
             $(beamdir)/cloudi_service_messaging_sequence1.beam \
             $(beamdir)/cloudi_service_messaging_sequence2.beam \
             $(beamdir)/cloudi_service_messaging_sequence3.beam \
             $(beamdir)/cloudi_service_messaging_sequence4.beam

all-local: $(beam_DATA) $(srcdir)/$(APPLICATION)
	$(MKDIR_P) $(beamdir) || exit 0
	cp -f $(srcdir)/$(APPLICATION) $(beamdir)
	cp -f $(beam_DATA) $(beamdir)

.erl.beam:
	@ERLC@ -b beam \
           -pz $(top_builddir)/lib/cloudi_core/ebin \
           $(ERLC_OPTS) -o $@ $<

install-exec-hook: $(beamdir)/$(APPLICATION) \
                   $(beamdir)/cloudi_service_messaging_sequence1.beam \
                   $(beamdir)/cloudi_service_messaging_sequence2.beam \
                   $(beamdir)/cloudi_service_messaging_sequence3.beam \
                   $(beamdir)/cloudi_service_messaging_sequence4.beam
	$(MKDIR_P) $(instdir)
	$(INSTALL_DATA) $(beamdir)/$(APPLICATION) $(instdir)
	$(INSTALL_DATA) $(beamdir)/cloudi_service_messaging_sequence1.beam \
                    $(instdir)
	$(INSTALL_DATA) $(beamdir)/cloudi_service_messaging_sequence2.beam \
                    $(instdir)
	$(INSTALL_DATA) $(beamdir)/cloudi_service_messaging_sequence3.beam \
                    $(instdir)
	$(INSTALL_DATA) $(beamdir)/cloudi_service_messaging_sequence4.beam \
                    $(instdir)
