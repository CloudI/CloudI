#-*-Mode:make;coding:utf-8;tab-width:4;c-basic-offset:4-*-
# ex: set ft=make fenc=utf-8 sts=4 ts=4 sw=4 noet nomod:

instdir = "$(DESTDIR)$(cloudi_prefix)/service_api/dashboard/js"

install-exec-hook:
	$(MKDIR_P) $(instdir)
	$(INSTALL_DATA) $(srcdir)/dataTables.jqueryui.js $(instdir)
	$(INSTALL_DATA) $(srcdir)/jquery-1.11.1.min.js $(instdir)
	$(INSTALL_DATA) $(srcdir)/jquery.dataTables.min.js $(instdir)
	$(INSTALL_DATA) $(srcdir)/common.js $(instdir)

