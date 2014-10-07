#-*-Mode:make;coding:utf-8;tab-width:4;c-basic-offset:4-*-
# ex: set ft=make fenc=utf-8 sts=4 ts=4 sw=4 noet:

INTERFACE_HEADER = $(srcdir)/../src/cloudi_core_i_os_spawn.hrl
CURRENT_VERSION = vsn_1

instdir = $(DESTDIR)$(cloudi_prefix)/lib/cloudi_core-$(VERSION)/priv
inst_PROGRAMS = cloudi_os_spawn_vsn_1
inst_LTLIBRARIES = libcloudi_socket_drv.la

BUILT_SOURCES = $(INTERFACE_HEADER)
CLEANFILES = $(INTERFACE_HEADER)
$(INTERFACE_HEADER): Makefile \
                     cloudi_os_spawn_hrl.h \
                     cloudi_os_spawn.h
	$(CXX) -DCURRENT_VERSION=$(CURRENT_VERSION) \
         -include $(srcdir)/cloudi_os_spawn.h \
         $(BOOST_CPPFLAGS) -E -P $(srcdir)/cloudi_os_spawn_hrl.h > $@

cloudi_os_spawn_vsn_1_SOURCES = port.cpp os_spawn.cpp assert.cpp
cloudi_os_spawn_vsn_1_CPPFLAGS = \
 -I$(ERLANG_LIB_DIR_erl_interface)/include/ \
 -I$(ERLANG_ROOT_DIR)/erts-$(ERLANG_ERTS_VER)/include/ \
 -DCURRENT_VERSION=$(CURRENT_VERSION) $(BOOST_CPPFLAGS) \
 -include $(srcdir)/cloudi_os_spawn.h $(CXXFLAGS)
cloudi_os_spawn_vsn_1_LDADD = -lei
cloudi_os_spawn_vsn_1_LDFLAGS = -L$(ERLANG_LIB_DIR_erl_interface)/lib/

# no symbols need to be linked, since the Erlang VM already has the
# necessary functions
libcloudi_socket_drv_la_SOURCES = cloudi_socket_drv.cpp assert.cpp
libcloudi_socket_drv_la_CPPFLAGS = \
 -fPIC -I$(ERLANG_ROOT_DIR)/erts-$(ERLANG_ERTS_VER)/include/ \
 $(BOOST_CPPFLAGS) $(CXXFLAGS)
libcloudi_socket_drv_la_LDFLAGS = -module -avoid-version \
 -export-symbols-regex '(libcloudi_socket_drv|nif_init|on_load|on_unload)'

