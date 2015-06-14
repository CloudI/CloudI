#-*-Mode:make;coding:utf-8;tab-width:4;c-basic-offset:4-*-
# ex: set ft=make fenc=utf-8 sts=4 ts=4 sw=4 noet nomod:

SUBDIRS = images

instdir = "$(DESTDIR)$(cloudi_prefix)/service_api/dashboard/css"

install-exec-hook:
	$(MKDIR_P) $(instdir)
	$(INSTALL_DATA) $(srcdir)/dataTables.jqueryui.css $(instdir)
	$(INSTALL_DATA) $(srcdir)/jquery-ui.css $(instdir)

