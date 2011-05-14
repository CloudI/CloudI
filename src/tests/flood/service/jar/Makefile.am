
nodist_noinst_SCRIPTS = flood.jar

CLEANFILES = $(nodist_noinst_SCRIPTS)

flood.jar: manifest.txt \
           org/cloudi/tests/flood/*.class \
           $(top_builddir)/api/java/jar/cloudi.jar
	cp $(top_builddir)/api/java/jar/cloudi.jar .
	$(JAR) cvfm flood.jar $(srcdir)/manifest.txt org

clean-local:
	rm -rf org cloudi.jar
