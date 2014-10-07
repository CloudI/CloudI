#-*-Mode:make;coding:utf-8;tab-width:4;c-basic-offset:4-*-
# ex: set ft=make fenc=utf-8 sts=4 ts=4 sw=4 noet:

instdir = "$(DESTDIR)$(cloudi_prefix)/tests/service_api"

install-exec-hook:
	$(MKDIR_P) $(instdir)
	$(INSTALL_SCRIPT) $(srcdir)/run.py $(instdir)
	$(INSTALL_SCRIPT) $(srcdir)/logging_on.py $(instdir)
	$(INSTALL_SCRIPT) $(srcdir)/logging_off.py $(instdir)
	$(INSTALL_SCRIPT) $(srcdir)/path.py $(instdir)

