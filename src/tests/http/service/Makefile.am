#-*-Mode:make;coding:utf-8;tab-width:4;c-basic-offset:4-*-
# ex: set ft=make fenc=utf-8 sts=4 ts=4 sw=4 noet nomod:

if JAVA_SUPPORT
    SUBDIRS = org jar
endif
if PYTHON_SUPPORT
    PYTHON_INSTALL_HOOK = python-install
endif
if RUBY_SUPPORT
    RUBY_INSTALL_HOOK = ruby-install
endif
.PHONY: \
    python-install \
    ruby-install

instdir = "$(DESTDIR)$(cloudi_prefix)/tests/http/service"

python-install:
	$(MKDIR_P) $(instdir)
	$(INSTALL_SCRIPT) $(srcdir)/service.py $(instdir)

ruby-install:
	$(MKDIR_P) $(instdir)
	$(INSTALL_SCRIPT) $(srcdir)/service.rb $(instdir)

install-exec-hook: $(PYTHON_INSTALL_HOOK) $(RUBY_INSTALL_HOOK)

