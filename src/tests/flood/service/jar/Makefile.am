
nodist_noinst_SCRIPTS = flood.jar

CLEANFILES = $(nodist_libexec_SCRIPTS)

flood.jar: manifest.txt \
           org/cloudi/tests/flood/*.class \
           $(top_builddir)/api/java/jar/cloudi.jar
	cp $(top_builddir)/api/java/jar/cloudi.jar .
	jar cvfm flood.jar manifest.txt org

clean-local:
	rm -rf org cloudi.jar
