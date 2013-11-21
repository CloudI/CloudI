beamdir = ../ebin
beam_DATA = cloudi_service_messaging_sequence1.beam \
            cloudi_service_messaging_sequence2.beam \
            cloudi_service_messaging_sequence3.beam \
            cloudi_service_messaging_sequence4.beam
CLEANFILES = $(beam_DATA) \
             $(beamdir)/cloudi_service_messaging_sequence1.beam \
             $(beamdir)/cloudi_service_messaging_sequence2.beam \
             $(beamdir)/cloudi_service_messaging_sequence3.beam \
             $(beamdir)/cloudi_service_messaging_sequence4.beam

all-local: $(beam_DATA)
	$(MKDIR_P) $(beamdir) || exit 0
	cp -f $(beam_DATA) $(beamdir)

.erl.beam:
	@ERLC@ -b beam -I $(top_srcdir)/lib/cloudi_core/include \
           -pz $(top_builddir)/lib/cloudi_core/ebin \
           -o $@ $<

