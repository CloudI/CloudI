beamdir = ../ebin
beam_DATA = cloud_job_hexpi.beam
EXTRA_DIST = cloud_job_hexpi.erl
CLEANFILES = $(beam_DATA) \
             $(beamdir)/cloud_job_hexpi.beam

all-local: $(beam_DATA)
	cp $(beam_DATA) $(beamdir)

.erl.beam:
	@ERLC@ -b beam -I $(top_builddir)/lib/cloudi/include \
           -o $@ $<
