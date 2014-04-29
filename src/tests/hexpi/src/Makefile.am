beamdir = ../ebin
APPLICATION = cloudi_service_hexpi.app
beam_DATA = cloudi_service_hexpi.beam

CLEANFILES = $(beam_DATA) \
             $(beamdir)/cloudi_service_hexpi.beam

all-local: $(beam_DATA) $(APPLICATION)
	$(MKDIR_P) $(beamdir) || exit 0
	cp -f $(APPLICATION) $(beamdir)
	cp -f $(beam_DATA) $(beamdir)

.erl.beam:
	@ERLC@ -b beam -I $(top_srcdir)/lib/cloudi_core/include \
           -pz $(top_builddir)/lib/cloudi_core/ebin \
           -o $@ $<

