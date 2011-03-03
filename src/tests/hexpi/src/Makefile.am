beamdir = ../ebin
beam_DATA = cloudi_job_hexpi.beam
CLEANFILES = $(beam_DATA) \
             $(beamdir)/cloudi_job_hexpi.beam

all-local: $(beam_DATA)
	cp $(beam_DATA) $(beamdir)

.erl.beam:
	@ERLC@ -b beam -I $(top_builddir)/lib/cloudi/include -o $@ $<

