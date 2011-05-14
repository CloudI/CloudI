
nodist_libexec_SCRIPTS = service.jar

CLEANFILES = $(nodist_libexec_SCRIPTS)

service.jar: manifest.txt \
             org/cloudi/tests/http/*.class \
             $(top_builddir)/api/java/jar/cloudi.jar
	cp $(top_builddir)/api/java/jar/cloudi.jar .
	jar cvfm service.jar manifest.txt org

clean-local:
	rm -rf org cloudi.jar
