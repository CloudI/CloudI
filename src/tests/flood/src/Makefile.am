beamdir = ../ebin
beam_DATA = cloudi_service_flood.beam
CLEANFILES = $(beam_DATA) \
             $(beamdir)/cloudi_service_flood.beam

all-local: $(beam_DATA)
	$(MKDIR_P) $(beamdir) || exit 0
	cp -f $(beam_DATA) $(beamdir)

.erl.beam:
	@ERLC@ -b beam \
           -pz $(top_builddir)/lib/cloudi_core/ebin \
           $(ERLC_OPTS) -o $@ $<

