#-*-Mode:make;coding:utf-8;tab-width:4;c-basic-offset:4-*-
# ex: set ft=make fenc=utf-8 sts=4 ts=4 sw=4 noet:

if PYTHON_SUPPORT
    PYTHON_SUBDIR = python
endif
SUBDIRS = $(PYTHON_SUBDIR)

install-exec-hook:
	$(MKDIR_P) "$(DESTDIR)$(cloudi_prefix)/service_api"

