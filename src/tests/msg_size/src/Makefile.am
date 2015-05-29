#-*-Mode:make;coding:utf-8;tab-width:4;c-basic-offset:4-*-
# ex: set ft=make fenc=utf-8 sts=4 ts=4 sw=4 noet:

instdir = "$(DESTDIR)$(cloudi_prefix)/tests/msg_size/ebin"
beamdir = "$(top_builddir)/tests/msg_size/ebin"
beam_DATA = cloudi_service_msg_size.beam
CLEANFILES = $(beam_DATA) \
             $(beamdir)/cloudi_service_msg_size.beam

all-local: $(beam_DATA)
	$(MKDIR_P) $(beamdir) || exit 0
	cp -f $(beam_DATA) $(beamdir)

.erl.beam:
	@ERLC@ -b beam \
           -pz $(top_builddir)/lib/cloudi_core/ebin \
           $(ERLC_OPTS) -o $@ $<

install-exec-hook: $(beamdir)/cloudi_service_msg_size.beam
	$(MKDIR_P) $(instdir)
	$(INSTALL_DATA) $(beamdir)/cloudi_service_msg_size.beam $(instdir)

