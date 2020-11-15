#-*-Mode:make;coding:utf-8;tab-width:4;c-basic-offset:4-*-
# ex: set ft=make fenc=utf-8 sts=4 ts=4 sw=4 noet nomod:

INTERFACE_HEADER_SPAWN = $(srcdir)/../src/cloudi_core_i_os_spawn.hrl
INTERFACE_HEADER_COMMAND = $(srcdir)/../src/cloudi_core_i_os_command.hrl
RLIMIT_HEADER = $(srcdir)/../src/cloudi_core_i_os_rlimit.hrl

instdir = "$(cloudi_prefix)/lib/cloudi_core-$(VERSION)/priv"
inst_PROGRAMS = cloudi_os_spawn \
                cloudi_os_command

BUILT_SOURCES = \
    $(INTERFACE_HEADER_SPAWN) \
    $(INTERFACE_HEADER_COMMAND) \
    $(RLIMIT_HEADER)
CLEANFILES = $(BUILT_SOURCES)

$(INTERFACE_HEADER_SPAWN): Makefile \
                           cloudi_os_port_hrl.hpp \
                           cloudi_os_spawn_api.hpp
	$(CXXCPP) -include $(srcdir)/cloudi_os_spawn_api.hpp \
         $(BOOST_CPPFLAGS) -P $(srcdir)/cloudi_os_port_hrl.hpp > $@

$(INTERFACE_HEADER_COMMAND): Makefile \
                             cloudi_os_port_hrl.hpp \
                             cloudi_os_command_api.hpp
	$(CXXCPP) -include $(srcdir)/cloudi_os_command_api.hpp \
         $(BOOST_CPPFLAGS) -P $(srcdir)/cloudi_os_port_hrl.hpp > $@

$(RLIMIT_HEADER): Makefile \
                  cloudi_os_rlimit_hrl.hpp \
                  $(abs_top_builddir)/config.h
	$(CXXCPP) \
         -include $(abs_top_builddir)/config.h \
         $(BOOST_CPPFLAGS) -P $(srcdir)/cloudi_os_rlimit_hrl.hpp > $@

cloudi_os_spawn_SOURCES = cloudi_os_spawn.cpp \
                          cloudi_os_rlimit.cpp \
                          cloudi_os_owner.cpp \
                          cloudi_os_syscall_lock.cpp \
                          assert.cpp \
                          port.cpp
cloudi_os_spawn_CPPFLAGS = \
    -I$(ERLANG_LIB_DIR_erl_interface)/include/ \
    -I$(ERLANG_ROOT_DIR)/erts-$(ERLANG_ERTS_VER)/include/ \
    $(BOOST_CPPFLAGS) \
    -include $(srcdir)/cloudi_os_spawn_api.hpp $(CXXFLAGS)
cloudi_os_spawn_LDFLAGS = -L$(ERLANG_LIB_DIR_erl_interface)/lib/ \
                          $(SYSCALL_LOCK_LDFLAGS)
cloudi_os_spawn_LDADD = -lei $(SYSCALL_LOCK_LIB)

cloudi_os_command_SOURCES = cloudi_os_command.cpp \
                            assert.cpp \
                            port.cpp
cloudi_os_command_CPPFLAGS = \
    -I$(ERLANG_LIB_DIR_erl_interface)/include/ \
    -I$(ERLANG_ROOT_DIR)/erts-$(ERLANG_ERTS_VER)/include/ \
    $(BOOST_CPPFLAGS) \
    -include $(srcdir)/cloudi_os_command_api.hpp $(CXXFLAGS)
cloudi_os_command_LDFLAGS = -L$(ERLANG_LIB_DIR_erl_interface)/lib/
cloudi_os_command_LDADD = -lei

