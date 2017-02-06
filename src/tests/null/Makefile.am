#-*-Mode:make;coding:utf-8;tab-width:4;c-basic-offset:4-*-
# ex: set ft=make fenc=utf-8 sts=4 ts=4 sw=4 noet nomod:

if C_SUPPORT
    C_SUBDIR = c_src
endif
if GO_SUPPORT
    GO_SUBDIR = gopath
endif
if JAVA_SUPPORT
    JAVA_SUBDIR = org jar
endif
if JAVASCRIPT_SUPPORT
    JAVASCRIPT_INSTALL_HOOK = javascript-install
endif
if PERL_SUPPORT
    PERL_INSTALL_HOOK = perl-install
endif
if PHP_SUPPORT
    PHP_INSTALL_HOOK = php-install
endif
if PYTHON_SUPPORT
    PYTHON_INSTALL_HOOK = python-install
endif
if PYTHON_C_SUPPORT
    PYTHON_C_INSTALL_HOOK = python-c-install
endif
if RUBY_SUPPORT
    RUBY_INSTALL_HOOK = ruby-install
endif
.PHONY: \
    javascript-install \
    perl-install \
    php-install \
    python-install \
    python-c-install \
    ruby-install

SUBDIRS = $(C_SUBDIR) $(GO_SUBDIR) $(JAVA_SUBDIR) src
instdir = "$(DESTDIR)$(cloudi_prefix)/tests/null"

javascript-install:
	$(MKDIR_P) $(instdir)
	$(INSTALL_SCRIPT) $(srcdir)/null.js $(instdir)

perl-install:
	$(MKDIR_P) $(instdir)
	$(INSTALL_SCRIPT) $(srcdir)/null.pl $(instdir)

php-install:
	$(MKDIR_P) $(instdir)
	$(INSTALL_SCRIPT) $(srcdir)/null.php $(instdir)

python-install:
	$(MKDIR_P) $(instdir)
	$(INSTALL_SCRIPT) $(srcdir)/null.py $(instdir)

python-c-install:
	$(MKDIR_P) $(instdir)
	$(INSTALL_SCRIPT) $(srcdir)/null_c.py $(instdir)

ruby-install:
	$(MKDIR_P) $(instdir)
	$(INSTALL_SCRIPT) $(srcdir)/null.rb $(instdir)

install-exec-hook: $(JAVASCRIPT_INSTALL_HOOK) \
                   $(PERL_INSTALL_HOOK) \
                   $(PHP_INSTALL_HOOK) \
                   $(PYTHON_INSTALL_HOOK) \
                   $(PYTHON_C_INSTALL_HOOK) \
                   $(RUBY_INSTALL_HOOK)

