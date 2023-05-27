#-*-Mode:make;coding:utf-8;tab-width:4;c-basic-offset:4-*-
# ex: set ft=make fenc=utf-8 sts=4 ts=4 sw=4 noet nomod:

if ATS2_SUPPORT
    ATS2_SUBDIR = ats/v2
endif
if C_SUPPORT
    C_SUBDIR = c
endif
if GO_SUPPORT
    GO_SUBDIR = go
endif
if HASKELL_SUPPORT
    HASKELL_SUBDIR = haskell
endif
if JAVA_SUPPORT
    JAVA_SUBDIR = java
endif
if JAVASCRIPT_SUPPORT
    JAVASCRIPT_SUBDIR = javascript
endif
if OCAML_SUPPORT
    OCAML_SUBDIR = ocaml
endif
if PERL_SUPPORT
    PERL_SUBDIR = perl
endif
if PHP_SUPPORT
    PHP_SUBDIR = php
endif
if PYTHON_SUPPORT
    PYTHON_SUBDIR = python
endif
if RUBY_SUPPORT
    RUBY_SUBDIR = ruby
endif
if RUST_SUPPORT
    RUST_SUBDIR = rust
endif
SUBDIRS = $(C_SUBDIR) \
          $(ATS2_SUBDIR) erlang $(GO_SUBDIR) $(HASKELL_SUBDIR) \
          $(JAVA_SUBDIR) $(JAVASCRIPT_SUBDIR) \
          $(OCAML_SUBDIR) \
          $(PERL_SUBDIR) $(PHP_SUBDIR) \
          $(PYTHON_SUBDIR) $(RUBY_SUBDIR) $(RUST_SUBDIR)
DIST_SUBDIRS = $(SUBDIRS)

install-exec-hook:
	$(MKDIR_P) "$(DESTDIR)$(cloudi_prefix)/api"

