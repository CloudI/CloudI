#-*-Mode:make;coding:utf-8;tab-width:4;c-basic-offset:4-*-
# ex: set ft=make fenc=utf-8 sts=4 ts=4 sw=4 noet nomod:

instdir = "$(DESTDIR)$(cloudi_prefix)/api/java"
nodist_noinst_SCRIPTS = cloudi.jar

CLEANFILES = $(nodist_noinst_SCRIPTS)

cloudi.jar: org/cloudi/*.class \
            com/ericsson/otp/erlang/*.class
	$(JAR) cvf cloudi.jar com org

clean-local:
	rm -rf org com

install-exec-hook: cloudi.jar
	rmdir "$(DESTDIR)$(datarootdir)/java" || exit 0
	$(MKDIR_P) $(instdir)
	$(INSTALL_DATA) cloudi.jar $(instdir)

