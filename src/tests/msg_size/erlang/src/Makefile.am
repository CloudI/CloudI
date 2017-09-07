#-*-Mode:make;coding:utf-8;tab-width:4;c-basic-offset:4-*-
# ex: set ft=make fenc=utf-8 sts=4 ts=4 sw=4 noet nomod:

instdir = "$(cloudi_prefix)/tests/msg_size/erlang/ebin"
directinstdir = "$(DESTDIR)$(instdir)"
beamdir = "$(instdir)"
beam_DATA = cloudi_service_test_msg_size.beam
CLEANFILES = $(beam_DATA)
SUFFIXES = .erl .beam

all-local: $(beam_DATA)

.erl.beam:
	@ERLC@ -b beam \
           -pz $(top_builddir)/lib/cloudi_core/ebin \
           $(ERLC_OPTS) -o $@ $<

install-exec-hook: $(beam_DATA)
	$(MKDIR_P) $(directinstdir)
	$(INSTALL_DATA) cloudi_service_test_msg_size.beam $(directinstdir)

