#-*-Mode:make;coding:utf-8;tab-width:4;c-basic-offset:4-*-
# ex: set ft=make fenc=utf-8 sts=4 ts=4 sw=4 noet:

if CXX_SUPPORT
    CXX_SUBDIR = hexpi
endif
if PYTHON_SUPPORT
    PYTHON_SUBDIR = zeromq
endif
if PYTHON_C_SUPPORT
    PYTHON_C_SUBDIR = echo environment websockets
endif
SUBDIRS = \
    $(CXX_SUBDIR) $(PYTHON_SUBDIR) $(PYTHON_C_SUBDIR) \
    http http_req messaging msg_size request_rate

install-exec-hook:
	$(MKDIR_P) "$(DESTDIR)$(cloudi_prefix)/tests"

