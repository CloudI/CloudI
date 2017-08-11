#-*-Mode:make;coding:utf-8;tab-width:4;c-basic-offset:4-*-
# ex: set ft=make fenc=utf-8 sts=4 ts=4 sw=4 noet nomod:

instdir = "$(cloudi_prefix)/tests/hexpi/erlang/ebin"
directinstdir = "$(DESTDIR)$(instdir)"
beamdir = "$(instdir)"
beam_DATA = cloudi_service_hexpi.beam
CLEANFILES = $(beam_DATA)
SUFFIXES = .erl .beam

all-local: $(beam_DATA)

.erl.beam:
	$(ERLC) -b beam \
           -pz $(top_builddir)/lib/cloudi_core/ebin \
           -pz $(top_builddir)/lib/cloudi_service_map_reduce/ebin \
           $(ERLC_OPTS) -o $@ $<

install-exec-hook: $(beam_DATA) $(srcdir)/cloudi_service_hexpi.app
	$(MKDIR_P) $(directinstdir)
	$(INSTALL_DATA) $(srcdir)/cloudi_service_hexpi.app $(directinstdir)
	$(INSTALL_DATA) cloudi_service_hexpi.beam $(directinstdir)

