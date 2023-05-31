#-*-Mode:make;coding:utf-8;tab-width:4;c-basic-offset:4-*-
# ex: set ft=make fenc=utf-8 sts=4 ts=4 sw=4 noet nomod:

instdir = "$(DESTDIR)$(cloudi_prefix)/tests/null"
CLEANFILES = null_rust
RUSTC_LIBS = --extern cloudi=$(top_builddir)/api/rust/libcloudi.rlib \
             --extern erlang=$(top_builddir)/api/rust/liberlang.rlib

all-local: null_rust

null_rust: main.rs
	$(RUSTC) $(RUSTC_FLAGS) $(RUSTC_LIBS) $(srcdir)/main.rs -o $@

install-exec-hook: null_rust
	$(MKDIR_P) $(instdir)
	$(INSTALL_SCRIPT) null_rust $(instdir)

