#-*-Mode:make;coding:utf-8;tab-width:4;c-basic-offset:4-*-
# ex: set ft=make fenc=utf-8 sts=4 ts=4 sw=4 noet nomod:

instdir = "$(DESTDIR)$(cloudi_prefix)/service_api/python/jsonrpclib"

install-exec-hook:
	$(MKDIR_P) $(instdir)
	$(MKDIR_P) $(instdir)/jsonrpclib
	$(INSTALL_SCRIPT) $(srcdir)/README.md $(instdir)
	$(INSTALL_SCRIPT) $(srcdir)/LICENSE.txt $(instdir)
	$(INSTALL_SCRIPT) $(srcdir)/jsonrpclib/__init__.py $(instdir)/jsonrpclib
	$(INSTALL_SCRIPT) $(srcdir)/jsonrpclib/jsonrpc.py $(instdir)/jsonrpclib
	$(INSTALL_SCRIPT) $(srcdir)/jsonrpclib/config.py $(instdir)/jsonrpclib
	$(INSTALL_SCRIPT) $(srcdir)/jsonrpclib/history.py $(instdir)/jsonrpclib
	$(INSTALL_SCRIPT) $(srcdir)/jsonrpclib/jsonclass.py $(instdir)/jsonrpclib

