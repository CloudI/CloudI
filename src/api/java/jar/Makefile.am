
dist_libexec_SCRIPTS = cloudi.jar

CLEANFILES = $(dist_libexec_SCRIPTS)

cloudi.jar: org/cloudi/*.class \
            com/ericsson/otp/erlang/*.class
	$(JAR) cvf cloudi.jar com org

clean-local:
	rm -rf org com

