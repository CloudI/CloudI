#-*-Mode:make;coding:utf-8;tab-width:4;c-basic-offset:4-*-
# ex: set ft=make fenc=utf-8 sts=4 ts=4 sw=4 noet:

instdir = "$(DESTDIR)$(cloudi_prefix)/tests/http_req/jar"
nodist_noinst_SCRIPTS = http_req.jar

CLEANFILES = $(nodist_noinst_SCRIPTS)

cloudi.jar: $(top_builddir)/api/java/jar/cloudi.jar
	cp $(top_builddir)/api/java/jar/cloudi.jar .

http_req.jar: manifest.txt \
             org/cloudi/tests/http_req/*.class \
             cloudi.jar
	$(JAR) cvfm http_req.jar $(srcdir)/manifest.txt org

clean-local:
	rm -rf org cloudi.jar

install-exec-hook: http_req.jar cloudi.jar
	$(MKDIR_P) $(instdir)
	$(INSTALL_DATA) cloudi.jar $(instdir)
	$(INSTALL_DATA) http_req.jar $(instdir)
	
