#-*-Mode:make;coding:utf-8;tab-width:4;c-basic-offset:4-*-
# ex: set ft=make fenc=utf-8 sts=4 ts=4 sw=4 noet:

if C_SUPPORT
    C_SUBDIR = c
endif
if JAVA_SUPPORT
    JAVA_SUBDIR = java
endif
if PHP_SUPPORT
    PHP_SUBDIR = php
endif
if PYTHON_SUPPORT
    PYTHON_SUBDIR = python
endif
if RUBY_SUPPORT
    RUBY_SUBDIR = ruby
endif
SUBDIRS = $(C_SUBDIR) erlang $(JAVA_SUBDIR) $(PHP_SUBDIR) \
          $(PYTHON_SUBDIR) $(RUBY_SUBDIR)

install-exec-hook:
	$(MKDIR_P) "$(DESTDIR)$(cloudi_prefix)/api"

