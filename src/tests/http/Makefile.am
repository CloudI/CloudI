#-*-Mode:make;coding:utf-8;tab-width:4;c-basic-offset:4-*-
# ex: set ft=make fenc=utf-8 sts=4 ts=4 sw=4 noet nomod:

if JAVA_SUPPORT
    JAVA_SUBDIR = java
endif
if PYTHON_SUPPORT
    PYTHON_INSTALL_HOOK = python-install
endif
if RUBY_SUPPORT
    RUBY_INSTALL_HOOK = ruby-install
endif
SUBDIRS = $(JAVA_SUBDIR)
instdir = "$(DESTDIR)$(cloudi_prefix)/tests/http"

.PHONY: \
    python-install \
	ruby-install

clean-local:
	rm -f tmp/*

python-install:
	$(MKDIR_P) $(instdir)
	$(INSTALL_SCRIPT) $(srcdir)/http.py $(instdir)

ruby-install:
	$(MKDIR_P) $(instdir)
	$(INSTALL_SCRIPT) $(srcdir)/http.rb $(instdir)

install-exec-hook: $(PYTHON_INSTALL_HOOK) \
                   $(RUBY_INSTALL_HOOK)
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
