
nodist_noinst_SCRIPTS = messaging.jar

CLEANFILES = $(nodist_noinst_SCRIPTS)

messaging.jar: manifest.txt \
               org/cloudi/tests/messaging/*.class \
               $(top_builddir)/api/java/jar/cloudi.jar
	cp $(top_builddir)/api/java/jar/cloudi.jar .
	$(JAR) cvfm messaging.jar $(srcdir)/manifest.txt org

clean-local:
	rm -rf org cloudi.jar
