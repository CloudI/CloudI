#-*-Mode:make;coding:utf-8;tab-width:4;c-basic-offset:4-*-
# ex: set ft=make fenc=utf-8 sts=4 ts=4 sw=4 noet nomod:

SUBDIRS = css js

instdir = "$(DESTDIR)$(cloudi_prefix)/service_api/dashboard"

install-exec-hook:
	$(MKDIR_P) $(instdir)
	$(INSTALL_DATA) $(srcdir)/code_path.html $(instdir)
	$(INSTALL_DATA) $(srcdir)/index.html $(instdir)
	$(INSTALL_DATA) $(srcdir)/log_settings.html $(instdir)
	$(INSTALL_DATA) $(srcdir)/nodes.html $(instdir)
	$(INSTALL_DATA) $(srcdir)/powered_by_cloudi.png $(instdir)
	$(INSTALL_DATA) $(srcdir)/README.md $(instdir)
	$(INSTALL_DATA) $(srcdir)/service_summary.html $(instdir)
	$(INSTALL_DATA) $(srcdir)/view_log.html $(instdir)

