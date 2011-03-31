beamdir = ../ebin
beam_DATA = cloudi_job_flood.beam
CLEANFILES = $(beam_DATA) \
             $(beamdir)/cloudi_job_flood.beam

all-local: $(beam_DATA)
	cp -f $(beam_DATA) $(beamdir)

.erl.beam:
	@ERLC@ -b beam -I $(top_builddir)/lib/cloudi/include \
           -pz $(top_builddir)/lib/cloudi/ebin \
           -o $@ $<

