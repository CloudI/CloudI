#-*-Mode:make;coding:utf-8;tab-width:4;c-basic-offset:4-*-
# ex: set ft=make fenc=utf-8 sts=4 ts=4 sw=4 noet nomod:

instdir = "$(DESTDIR)$(cloudi_prefix)/tests/http/service/jar"
nodist_noinst_SCRIPTS = service.jar

CLEANFILES = $(nodist_noinst_SCRIPTS)

cloudi.jar: $(top_builddir)/api/java/jar/cloudi.jar
	cp $(top_builddir)/api/java/jar/cloudi.jar .

service.jar: manifest.txt \
             org/cloudi/tests/http/*.class \
             cloudi.jar
	$(JAR) cvfm service.jar $(srcdir)/manifest.txt org

clean-local:
	rm -rf org cloudi.jar

install-exec-hook: service.jar cloudi.jar
	$(MKDIR_P) $(instdir)
	$(INSTALL_DATA) cloudi.jar $(instdir)
	$(INSTALL_DATA) service.jar $(instdir)

