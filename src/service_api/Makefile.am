#-*-Mode:make;coding:utf-8;tab-width:4;c-basic-offset:4-*-
# ex: set ft=make fenc=utf-8 sts=4 ts=4 sw=4 noet nomod:

if PYTHON_SUPPORT
    PYTHON_SUBDIR = python
endif
SUBDIRS = dashboard $(PYTHON_SUBDIR)
DIST_SUBDIRS = $(SUBDIRS)

install-exec-hook:
	$(MKDIR_P) "$(DESTDIR)$(cloudi_prefix)/service_api"

