#-*-Mode:make;coding:utf-8;tab-width:4;c-basic-offset:4-*-
# ex: set ft=make fenc=utf-8 sts=4 ts=4 sw=4 noet:

if CXX_SUPPORT
    CXX_SUBDIR = cxx_src
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
    php-install \
    python-install \
    python-c-install \
    ruby-install

SUBDIRS = $(CXX_SUBDIR) $(JAVA_SUBDIR) src
instdir = "$(DESTDIR)$(cloudi_prefix)/tests/msg_size"

php-install:
	$(MKDIR_P) $(instdir)
	$(INSTALL_SCRIPT) $(srcdir)/msg_size.php $(instdir)

python-install:
	$(MKDIR_P) $(instdir)
	$(INSTALL_SCRIPT) $(srcdir)/msg_size.py $(instdir)

python-c-install:
	$(MKDIR_P) $(instdir)
	$(INSTALL_SCRIPT) $(srcdir)/msg_size_c.py $(instdir)

ruby-install:
	$(MKDIR_P) $(instdir)
	$(INSTALL_SCRIPT) $(srcdir)/msg_size.rb $(instdir)

install-exec-hook: $(PHP_INSTALL_HOOK) \
                   $(PYTHON_INSTALL_HOOK) \
                   $(PYTHON_C_INSTALL_HOOK) \
                   $(RUBY_INSTALL_HOOK)
