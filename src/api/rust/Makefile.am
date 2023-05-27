#-*-Mode:make;coding:utf-8;tab-width:4;c-basic-offset:4-*-
# ex: set ft=make fenc=utf-8 sts=4 ts=4 sw=4 noet nomod:

instdir = "$(cloudi_prefix)/api/rust"
directinstdir = "$(DESTDIR)$(instdir)"
rustdir = $(instdir)
rust_DATA = liberlang.rlib libcloudi.rlib
CLEANFILES = $(rust_DATA)

all-local: $(rust_DATA)

libcloudi.rlib: liberlang.rlib $(srcdir)/cloudi.rs
	$(RUSTC) $(RUSTC_FLAGS) --extern erlang=liberlang.rlib \
             $(srcdir)/cloudi.rs -o $@

liberlang.rlib: $(srcdir)/erlang.rs
	$(RUSTC) $(RUSTC_FLAGS) $(srcdir)/erlang.rs -o $@

