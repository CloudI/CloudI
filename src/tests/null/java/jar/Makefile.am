#-*-Mode:make;coding:utf-8;tab-width:4;c-basic-offset:4-*-
# ex: set ft=make fenc=utf-8 sts=4 ts=4 sw=4 noet nomod:

instdir = "$(DESTDIR)$(cloudi_prefix)/tests/null/java"
CLEANFILES = null_.jar cloudi.jar

.NOTPARALLEL:

all-local: null_.jar cloudi.jar

cloudi.jar: $(top_builddir)/api/java/jar/cloudi.jar
	cp $(top_builddir)/api/java/jar/cloudi.jar .

null_.jar: manifest.txt \
           org/cloudi/tests/null_/Main.class \
           org/cloudi/tests/null_/Task.class
	$(JAR) cvfm null_.jar $(srcdir)/manifest.txt org

clean-local:
	rm -rf org

install-exec-hook: null_.jar cloudi.jar
	$(MKDIR_P) $(instdir)
	$(INSTALL_DATA) cloudi.jar $(instdir)
	$(INSTALL_DATA) null_.jar $(instdir)
	
