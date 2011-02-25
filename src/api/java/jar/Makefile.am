
dist_libexec_SCRIPTS = cloudi.jar

CLEANFILES = $(dist_libexec_SCRIPTS)

cloudi.jar: org/cloudi/*.class \
            com/ericsson/otp/erlang/*.class \
            manifest.txt
	jar cvfm cloudi.jar manifest.txt com org

clean-local:
	rm -rf org com

