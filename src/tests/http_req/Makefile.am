#-*-Mode:make;coding:utf-8;tab-width:4;c-basic-offset:4-*-
# ex: set ft=make fenc=utf-8 sts=4 ts=4 sw=4 noet:

if C_SUPPORT
    C_SUBDIR = c_src
endif
if JAVA_SUPPORT
    JAVA_SUBDIR = org jar
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
    python-install \
    python-c-install \
    ruby-install

SUBDIRS = $(C_SUBDIR) $(JAVA_SUBDIR) src
instdir = "$(DESTDIR)$(cloudi_prefix)/tests/http_req"

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

install-exec-hook: $(PHP_INSTALL_HOOK) \
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

