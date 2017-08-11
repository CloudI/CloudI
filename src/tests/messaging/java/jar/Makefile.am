#-*-Mode:make;coding:utf-8;tab-width:4;c-basic-offset:4-*-
# ex: set ft=make fenc=utf-8 sts=4 ts=4 sw=4 noet nomod:

instdir = "$(DESTDIR)$(cloudi_prefix)/tests/messaging/java"
CLEANFILES = messaging.jar cloudi.jar

.NOTPARALLEL:

all-local: messaging.jar cloudi.jar

cloudi.jar: $(top_builddir)/api/java/jar/cloudi.jar
	cp $(top_builddir)/api/java/jar/cloudi.jar .

messaging.jar: manifest.txt \
               org/cloudi/tests/messaging/Main.class \
               org/cloudi/tests/messaging/Task.class
	$(JAR) cvfm messaging.jar $(srcdir)/manifest.txt org

clean-local:
	rm -rf org

install-exec-hook: messaging.jar cloudi.jar
	$(MKDIR_P) $(instdir)
	$(INSTALL_DATA) cloudi.jar $(instdir)
	$(INSTALL_DATA) messaging.jar $(instdir)

