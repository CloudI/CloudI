#-*-Mode:make;coding:utf-8;tab-width:4;c-basic-offset:4-*-
# ex: set ft=make fenc=utf-8 sts=4 ts=4 sw=4 noet:

if JAVA_SUPPORT
    SCRIPTS_INSTALL_HOOK = scripts-install
    SUBDIRS = service
endif
if PYTHON_SUPPORT
    SCRIPTS_INSTALL_HOOK = scripts-install
    SUBDIRS = service
endif
if RUBY_SUPPORT
    SCRIPTS_INSTALL_HOOK = scripts-install
    SUBDIRS = service
endif
.PHONY: \
    scripts-install

instdir = "$(DESTDIR)$(cloudi_prefix)/tests/http"

clean-local:
	rm -f tmp/*

scripts-install:
	$(MKDIR_P) $(instdir)
	$(MKDIR_P) $(instdir)/input
	$(MKDIR_P) $(instdir)/input.zip
	$(MKDIR_P) $(instdir)/output
	$(MKDIR_P) $(instdir)/tmp
	$(INSTALL_SCRIPT) "$(top_srcdir)/tests/http/run.sh" $(instdir)
	$(INSTALL_DATA) "$(top_srcdir)/tests/http/input/text" \
	                $(instdir)/input/
	$(INSTALL_DATA) "$(top_srcdir)/tests/http/input.zip/text" \
	                $(instdir)/input.zip
	$(INSTALL_DATA) "$(top_srcdir)/tests/http/output/text" \
                    $(instdir)/output

install-exec-hook: $(SCRIPTS_INSTALL_HOOK)

