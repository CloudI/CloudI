
nodist_noinst_SCRIPTS = service.jar

CLEANFILES = $(nodist_noinst_SCRIPTS)

service.jar: manifest.txt \
             org/cloudi/tests/http/*.class \
             $(top_builddir)/api/java/jar/cloudi.jar
	cp $(top_builddir)/api/java/jar/cloudi.jar .
	$(JAR) cvfm service.jar $(srcdir)/manifest.txt org

clean-local:
	rm -rf org cloudi.jar
