#-*-Mode:make;coding:utf-8;tab-width:4;c-basic-offset:4-*-
# ex: set ft=make fenc=utf-8 sts=4 ts=4 sw=4 noet nomod:

if CXX_SUPPORT
    CXX_SUBDIR = hexpi
endif
if PYTHON_SUPPORT
    PYTHON_SUBDIR = service_api
endif
if PYTHON_C_SUPPORT
    PYTHON_C_SUBDIR = echo environment websockets
endif
if INTEGRATION_TESTS
    INTEGRATION_TESTS_SUBDIR = \
        $(CXX_SUBDIR) $(PYTHON_SUBDIR) $(PYTHON_C_SUBDIR) \
        count http http_req messaging msg_size null
endif
SUBDIRS = $(INTEGRATION_TESTS_SUBDIR)
DIST_SUBDIRS = $(SUBDIRS)

install-exec-hook:
	$(MKDIR_P) "$(DESTDIR)$(cloudi_prefix)/tests"

