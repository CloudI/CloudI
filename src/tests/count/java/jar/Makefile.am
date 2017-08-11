#-*-Mode:make;coding:utf-8;tab-width:4;c-basic-offset:4-*-
# ex: set ft=make fenc=utf-8 sts=4 ts=4 sw=4 noet nomod:

instdir = "$(DESTDIR)$(cloudi_prefix)/tests/count/java"
CLEANFILES = count.jar cloudi.jar

.NOTPARALLEL:

all-local: count.jar cloudi.jar

cloudi.jar: $(top_builddir)/api/java/jar/cloudi.jar
	cp $(top_builddir)/api/java/jar/cloudi.jar .

count.jar: manifest.txt \
           org/cloudi/tests/count/Main.class \
           org/cloudi/tests/count/Task.class
	$(JAR) cvfm count.jar $(srcdir)/manifest.txt org

clean-local:
	rm -rf org

install-exec-hook: count.jar cloudi.jar
	$(MKDIR_P) $(instdir)
	$(INSTALL_DATA) cloudi.jar $(instdir)
	$(INSTALL_DATA) count.jar $(instdir)

