#-*-Mode:make;coding:utf-8;tab-width:4;c-basic-offset:4-*-
# ex: set ft=make fenc=utf-8 sts=4 ts=4 sw=4 noet nomod:

if ATS2_SUPPORT
    ATS2_SUBDIR = ats/v2
endif
if CXX_SUPPORT
    CXX_SUBDIR = cxx
endif
if GO_SUPPORT
    GO_SUBDIR = gopath
endif
if HASKELL_SUPPORT
    HASKELL_SUBDIR = haskell
endif
if JAVA_SUPPORT
    JAVA_SUBDIR = java
endif
if JAVASCRIPT_SUPPORT
    JAVASCRIPT_INSTALL_HOOK = javascript-install
endif
if OCAML_SUPPORT
    OCAML_SUBDIR = ocaml
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
SUBDIRS = $(ATS2_SUBDIR) $(CXX_SUBDIR) $(GO_SUBDIR) $(HASKELL_SUBDIR) \
          $(JAVA_SUBDIR) $(OCAML_SUBDIR) erlang
DIST_SUBDIRS = $(SUBDIRS)
instdir = "$(DESTDIR)$(cloudi_prefix)/tests/messaging"

.PHONY: \
    javascript-install \
    perl-install \
    php-install \
    python-install \
    python-c-install \
    ruby-install

javascript-install:
	$(MKDIR_P) $(instdir)
	$(INSTALL_SCRIPT) $(srcdir)/messaging.js $(instdir)

perl-install:
	$(MKDIR_P) $(instdir)
	$(INSTALL_SCRIPT) $(srcdir)/MessagingTask.pm $(instdir)

php-install:
	$(MKDIR_P) $(instdir)
	$(INSTALL_SCRIPT) $(srcdir)/messaging.php $(instdir)

python-install:
	$(MKDIR_P) $(instdir)
	$(INSTALL_SCRIPT) $(srcdir)/messaging.py $(instdir)

python-c-install:
	$(MKDIR_P) $(instdir)
	$(INSTALL_SCRIPT) $(srcdir)/messaging_c.py $(instdir)

ruby-install:
	$(MKDIR_P) $(instdir)
	$(INSTALL_SCRIPT) $(srcdir)/messaging.rb $(instdir)

install-exec-hook: $(JAVASCRIPT_INSTALL_HOOK) \
                   $(PERL_INSTALL_HOOK) \
                   $(PHP_INSTALL_HOOK) \
                   $(PYTHON_INSTALL_HOOK) \
                   $(PYTHON_C_INSTALL_HOOK) \
                   $(RUBY_INSTALL_HOOK)

