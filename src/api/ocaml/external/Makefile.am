#-*-Mode:make;coding:utf-8;tab-width:4;c-basic-offset:4-*-
# ex: set ft=make fenc=utf-8 sts=4 ts=4 sw=4 noet nomod:

OCAMLDEPS_ZARITH_BYTE_CODE = \
    big_int_Z.cmi \
    libzarith.a \
    q.cmi \
    zarith.cma \
    zarith_top.cma \
    zarith_top.cmi \
    zarith_version.cmi \
    z.cmi
OCAMLDEPS_ZARITH_NATIVE_CODE = \
    big_int_Z.cmx \
    q.cmx \
    zarith.a \
    zarith.cmxa \
    zarith_version.cmx \
    z.cmx
OCAMLDEPS_ZARITH = \
    $(OCAMLDEPS_ZARITH_BYTE_CODE) \
    $(OCAMLDEPS_ZARITH_NATIVE_CODE)

CLEANFILES = $(OCAMLDEPS_ZARITH)

OCAML_ZARITH_MAKEFILE = MakefileCloudI

zarith.cmxa:
	cd $(srcdir)/zarith-1.12 && \
    $(MAKE) -f ./$(OCAML_ZARITH_MAKEFILE) \
            OCAMLC="$(OCAMLC)" \
            OCAMLOPT="$(OCAMLOPT)" \
            OCAMLDEP="$(OCAMLDEP)" \
            OCAMLMKLIB="$(OCAMLMKLIB)" \
            GMP_H_CFLAGS="$(GMP_H_CFLAGS)" \
            GMP_LIB="$(GMP_LIB)" \
            zarith.cmxa zarith.cma zarith_top.cma libzarith.a && \
    cp $(OCAMLDEPS_ZARITH) $(abs_builddir)

all-local: zarith.cmxa

clean-local:
	cd $(srcdir)/zarith-1.12 && \
    $(MAKE) -f ./$(OCAML_ZARITH_MAKEFILE) clean

