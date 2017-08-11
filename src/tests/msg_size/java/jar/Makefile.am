#-*-Mode:make;coding:utf-8;tab-width:4;c-basic-offset:4-*-
# ex: set ft=make fenc=utf-8 sts=4 ts=4 sw=4 noet nomod:

instdir = "$(DESTDIR)$(cloudi_prefix)/tests/msg_size/java"
CLEANFILES = msg_size.jar cloudi.jar

.NOTPARALLEL:

all-local: msg_size.jar cloudi.jar

cloudi.jar: $(top_builddir)/api/java/jar/cloudi.jar
	cp $(top_builddir)/api/java/jar/cloudi.jar .

msg_size.jar: manifest.txt \
              org/cloudi/tests/msg_size/Main.class \
              org/cloudi/tests/msg_size/Task.class
	$(JAR) cvfm msg_size.jar $(srcdir)/manifest.txt org

clean-local:
	rm -rf org

install-exec-hook: msg_size.jar cloudi.jar
	$(MKDIR_P) $(instdir)
	$(INSTALL_DATA) cloudi.jar $(instdir)
	$(INSTALL_DATA) msg_size.jar $(instdir)

