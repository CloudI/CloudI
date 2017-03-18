#-*-Mode:make;coding:utf-8;tab-width:4;c-basic-offset:4-*-
# ex: set ft=make fenc=utf-8 sts=4 ts=4 sw=4 noet nomod:

if C_SUPPORT
    C_SUBDIR = c_src
endif
if GO_SUPPORT
    GO_SUBDIR = gopath
endif
if HASKELL_SUPPORT
    HASKELL_SUBDIR = haskell
endif
if JAVA_SUPPORT
    JAVA_SUBDIR = org jar
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
.PHONY: \
    javascript-install \
    perl-install \
    php-install \
    python-install \
    python-c-install \
    ruby-install

SUBDIRS = $(C_SUBDIR) $(GO_SUBDIR) $(HASKELL_SUBDIR) \
          $(JAVA_SUBDIR) $(OCAML_SUBDIR) src
instdir = "$(DESTDIR)$(cloudi_prefix)/tests/http_req"

javascript-install:
	$(MKDIR_P) $(instdir)
	$(INSTALL_SCRIPT) $(srcdir)/http_req.js $(instdir)

perl-install:
	$(MKDIR_P) $(instdir)
	$(INSTALL_SCRIPT) $(srcdir)/http_req.pl $(instdir)

php-install:
	$(MKDIR_P) $(instdir)
	$(INSTALL_SCRIPT) $(srcdir)/http_req.php $(instdir)

python-install:
	$(MKDIR_P) $(instdir)
	$(INSTALL_SCRIPT) $(srcdir)/http_req.py $(instdir)

python-c-install:
	$(MKDIR_P) $(instdir)
	$(INSTALL_SCRIPT) $(srcdir)/http_req_c.py $(instdir)

ruby-install:
	$(MKDIR_P) $(instdir)
	$(INSTALL_SCRIPT) $(srcdir)/http_req.rb $(instdir)

install-exec-hook: $(JAVASCRIPT_INSTALL_HOOK) \
                   $(PERL_INSTALL_HOOK) \
                   $(PHP_INSTALL_HOOK) \
                   $(PYTHON_INSTALL_HOOK) \
                   $(PYTHON_C_INSTALL_HOOK) \
                   $(RUBY_INSTALL_HOOK)
	$(MKDIR_P) $(instdir)/public_html
	$(INSTALL_DATA) $(srcdir)/public_html/index.html \
                    $(instdir)/public_html
	$(INSTALL_DATA) $(srcdir)/public_html/websockets.html \
                    $(instdir)/public_html
	$(INSTALL_DATA) $(srcdir)/public_html/style.css \
                    $(instdir)/public_html
	$(INSTALL_DATA) $(srcdir)/public_html/800px-Juliadim2.png \
                    $(instdir)/public_html
	$(INSTALL_DATA) $(srcdir)/public_html/hexpi.txt \
                    $(instdir)/public_html

