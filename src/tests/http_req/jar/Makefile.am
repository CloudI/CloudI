
nodist_noinst_SCRIPTS = http_req.jar

CLEANFILES = $(nodist_noinst_SCRIPTS)

http_req.jar: manifest.txt \
             org/cloudi/tests/http_req/*.class \
             $(top_builddir)/api/java/jar/cloudi.jar
	cp $(top_builddir)/api/java/jar/cloudi.jar .
	$(JAR) cvfm http_req.jar $(srcdir)/manifest.txt org

clean-local:
	rm -rf org cloudi.jar
