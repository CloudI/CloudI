
nodist_noinst_SCRIPTS = cloudi.jar

CLEANFILES = $(nodist_noinst_SCRIPTS)

cloudi.jar: org/cloudi/*.class \
            com/ericsson/otp/erlang/*.class
	$(JAR) cvf cloudi.jar com org

clean-local:
	rm -rf org com

