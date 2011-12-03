
nodist_noinst_SCRIPTS = msg_size.jar

CLEANFILES = $(nodist_noinst_SCRIPTS)

msg_size.jar: manifest.txt \
              org/cloudi/tests/msg_size/*.class \
              $(top_builddir)/api/java/jar/cloudi.jar
	cp $(top_builddir)/api/java/jar/cloudi.jar .
	$(JAR) cvfm msg_size.jar $(srcdir)/manifest.txt org

clean-local:
	rm -rf org cloudi.jar
