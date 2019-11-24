#-*-Mode:make;coding:utf-8;tab-width:4;c-basic-offset:4-*-
# ex: set ft=make fenc=utf-8 sts=4 ts=4 sw=4 noet nomod:

INTERFACE_HEADER = $(srcdir)/../src/cloudi_core_i_os_spawn.hrl
RLIMIT_HEADER = $(srcdir)/../src/cloudi_core_i_os_rlimit.hrl

instdir = "$(cloudi_prefix)/lib/cloudi_core-$(VERSION)/priv"
inst_PROGRAMS = cloudi_os_spawn

BUILT_SOURCES = $(INTERFACE_HEADER) $(RLIMIT_HEADER)
CLEANFILES = $(INTERFACE_HEADER) $(RLIMIT_HEADER)

$(INTERFACE_HEADER): Makefile \
                     cloudi_os_spawn_hrl.h \
                     cloudi_os_spawn_api.h
	$(CXXCPP) -include $(srcdir)/cloudi_os_spawn_api.h \
         $(BOOST_CPPFLAGS) -P $(srcdir)/cloudi_os_spawn_hrl.h > $@

$(RLIMIT_HEADER): Makefile \
                  cloudi_os_rlimit_hrl.h \
                  $(abs_top_builddir)/config.h
	$(CXXCPP) \
         -include $(abs_top_builddir)/config.h \
         $(BOOST_CPPFLAGS) -P $(srcdir)/cloudi_os_rlimit_hrl.h > $@

cloudi_os_spawn_SOURCES = cloudi_os_spawn.cpp \
                          cloudi_os_rlimit.cpp \
                          cloudi_os_owner.cpp \
                          assert.cpp \
                          port.cpp
cloudi_os_spawn_CPPFLAGS = \
    -I$(ERLANG_LIB_DIR_erl_interface)/include/ \
    -I$(ERLANG_ROOT_DIR)/erts-$(ERLANG_ERTS_VER)/include/ \
    $(BOOST_CPPFLAGS) \
    -include $(srcdir)/cloudi_os_spawn_api.h $(CXXFLAGS)
cloudi_os_spawn_LDADD = -lei
cloudi_os_spawn_LDFLAGS = -L$(ERLANG_LIB_DIR_erl_interface)/lib/

