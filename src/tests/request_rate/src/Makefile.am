beamdir = ../ebin
APPLICATION = cloudi_service_request_rate.app
beam_DATA = cloudi_service_request_rate.beam
CLEANFILES = $(beam_DATA) \
             $(beamdir)/$(APPLICATION) \
             $(beamdir)/cloudi_service_request_rate.beam

all-local: $(beam_DATA) $(srcdir)/$(APPLICATION)
	$(MKDIR_P) $(beamdir) || exit 0
	cp -f $(srcdir)/$(APPLICATION) $(beamdir)
	cp -f $(beam_DATA) $(beamdir)

.erl.beam:
	@ERLC@ -b beam -I $(top_srcdir)/lib/cloudi_core/include \
           -pz $(top_builddir)/lib/cloudi_core/ebin \
           -o $@ $<

