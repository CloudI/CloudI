#-*-Mode:make;coding:utf-8;tab-width:4;c-basic-offset:4-*-
# ex: set ft=make fenc=utf-8 sts=4 ts=4 sw=4 noet:

instdir = "$(DESTDIR)$(cloudi_prefix)/tests/http_req/ebin"
beamdir = "$(top_builddir)/tests/http_req/ebin"
beam_DATA = cloudi_service_http_req.beam
CLEANFILES = $(beam_DATA) \
             $(beamdir)/cloudi_service_http_req.beam

all-local: $(beam_DATA)
	$(MKDIR_P) $(beamdir) || exit 0
	cp -f $(beam_DATA) $(beamdir)

.erl.beam:
	@ERLC@ -b beam \
           -pz $(top_builddir)/lib/cloudi_core/ebin \
           $(ERLC_OPTS) -o $@ $<

install-exec-hook: $(beamdir)/cloudi_service_http_req.beam
	$(MKDIR_P) $(instdir)
	$(INSTALL_DATA) $(beamdir)/cloudi_service_http_req.beam $(instdir)

