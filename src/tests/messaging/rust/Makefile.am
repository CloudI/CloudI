#-*-Mode:make;coding:utf-8;tab-width:4;c-basic-offset:4-*-
# ex: set ft=make fenc=utf-8 sts=4 ts=4 sw=4 noet nomod:

instdir = "$(DESTDIR)$(cloudi_prefix)/tests/messaging"
CLEANFILES = messaging_rust
RUSTC_LIBS = --extern cloudi=$(top_builddir)/api/rust/libcloudi.rlib \
             --extern erlang=$(top_builddir)/api/rust/liberlang.rlib

all-local: messaging_rust

messaging_rust: main.rs
	$(RUSTC) $(RUSTC_FLAGS) $(RUSTC_LIBS) $(srcdir)/main.rs -o $@

install-exec-hook: messaging_rust
	$(MKDIR_P) $(instdir)
	$(INSTALL_SCRIPT) messaging_rust $(instdir)

