#-*-Mode:make;coding:utf-8;tab-width:4;c-basic-offset:4-*-
# ex: set ft=make fenc=utf-8 sts=4 ts=4 sw=4 noet nomod:

instdir = "$(DESTDIR)$(cloudi_prefix)/tests/http_req/java"
CLEANFILES = http_req.jar cloudi.jar

.NOTPARALLEL:

all-local: http_req.jar cloudi.jar

cloudi.jar: $(top_builddir)/api/java/jar/cloudi.jar
	cp $(top_builddir)/api/java/jar/cloudi.jar .

http_req.jar: manifest.txt \
              org/cloudi/tests/http_req/Main.class \
              org/cloudi/tests/http_req/Task.class
	$(JAR) cvfm http_req.jar $(srcdir)/manifest.txt org

clean-local:
	rm -rf org

install-exec-hook: http_req.jar cloudi.jar
	$(MKDIR_P) $(instdir)
	$(INSTALL_DATA) cloudi.jar $(instdir)
	$(INSTALL_DATA) http_req.jar $(instdir)
	
