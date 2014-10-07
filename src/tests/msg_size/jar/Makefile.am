#-*-Mode:make;coding:utf-8;tab-width:4;c-basic-offset:4-*-
# ex: set ft=make fenc=utf-8 sts=4 ts=4 sw=4 noet:

instdir = "$(DESTDIR)$(cloudi_prefix)/tests/msg_size/jar"
nodist_noinst_SCRIPTS = msg_size.jar

CLEANFILES = $(nodist_noinst_SCRIPTS)

cloudi.jar: $(top_builddir)/api/java/jar/cloudi.jar
	cp $(top_builddir)/api/java/jar/cloudi.jar .

msg_size.jar: manifest.txt \
              org/cloudi/tests/msg_size/*.class \
              cloudi.jar
	$(JAR) cvfm msg_size.jar $(srcdir)/manifest.txt org

clean-local:
	rm -rf org cloudi.jar

install-exec-hook: msg_size.jar cloudi.jar
	$(MKDIR_P) $(instdir)
	$(INSTALL_DATA) cloudi.jar $(instdir)
	$(INSTALL_DATA) msg_size.jar $(instdir)

