
dist_libexec_SCRIPTS = cloudi.jar

CLEANFILES = $(dist_libexec_SCRIPTS)

cloudi.jar: org com manifest.txt
	jar cvfm cloudi.jar manifest.txt com org

clean-local:
	rm -rf org com

