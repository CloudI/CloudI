#-*-Mode:make;coding:utf-8;tab-width:4;c-basic-offset:4-*-
# ex: set ft=make fenc=utf-8 sts=4 ts=4 sw=4 noet nomod:

instdir = "$(cloudi_prefix)/tests/messaging/erlang/ebin"
directinstdir = "$(DESTDIR)$(instdir)"
beamdir = "$(instdir)"
beam_DATA = cloudi_service_messaging_sequence1.beam \
            cloudi_service_messaging_sequence2.beam \
            cloudi_service_messaging_sequence3.beam \
            cloudi_service_messaging_sequence4.beam
CLEANFILES = $(beam_DATA)
SUFFIXES = .erl .beam

all-local: $(beam_DATA)

.erl.beam:
	@ERLC@ -b beam \
           -pz $(top_builddir)/lib/cloudi_core/ebin \
           $(ERLC_OPTS) -o $@ $<

install-exec-hook: $(beam_DATA) $(srcdir)/cloudi_service_messaging_sequence.app
	$(MKDIR_P) $(directinstdir)
	$(INSTALL_DATA) $(srcdir)/cloudi_service_messaging_sequence.app $(directinstdir)
	$(INSTALL_DATA) cloudi_service_messaging_sequence1.beam $(directinstdir)
	$(INSTALL_DATA) cloudi_service_messaging_sequence2.beam $(directinstdir)
	$(INSTALL_DATA) cloudi_service_messaging_sequence3.beam $(directinstdir)
	$(INSTALL_DATA) cloudi_service_messaging_sequence4.beam $(directinstdir)
