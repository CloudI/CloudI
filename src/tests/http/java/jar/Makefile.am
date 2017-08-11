#-*-Mode:make;coding:utf-8;tab-width:4;c-basic-offset:4-*-
# ex: set ft=make fenc=utf-8 sts=4 ts=4 sw=4 noet nomod:

instdir = "$(DESTDIR)$(cloudi_prefix)/tests/http/java"
CLEANFILES = http.jar cloudi.jar

all-local: http.jar cloudi.jar

cloudi.jar: $(top_builddir)/api/java/jar/cloudi.jar
	cp $(top_builddir)/api/java/jar/cloudi.jar .

http.jar: manifest.txt \
          org/cloudi/tests/http/Main.class \
          org/cloudi/tests/http/Task.class
	$(JAR) cvfm http.jar $(srcdir)/manifest.txt org

clean-local:
	rm -rf org

install-exec-hook: http.jar cloudi.jar
	$(MKDIR_P) $(instdir)
	$(INSTALL_DATA) cloudi.jar $(instdir)
	$(INSTALL_DATA) http.jar $(instdir)

