#-*-Mode:make;coding:utf-8;tab-width:4;c-basic-offset:4-*-
# ex: set ft=make fenc=utf-8 sts=4 ts=4 sw=4 noet nomod:

OCAMLDEPS_NUM = \
    arith_flags.cmi \
    arith_flags.cmx \
    arith_status.cmi \
    arith_status.cmx \
    big_int.cmi \
    big_int.cmx \
    int_misc.cmi \
    int_misc.cmx \
    nat.cmi \
    nat.cmx \
    num.cmi \
    num.cmx \
    ratio.cmi \
    ratio.cmx \
    libnums.a \
    nums.a \
    nums.cmxa
CLEANFILES = $(OCAMLDEPS_NUM)

nums.cmxa:
	cd $(srcdir)/num-1.1/src && \
    $(MAKE) OCAMLC="$(OCAMLC)" \
            OCAMLOPT="$(OCAMLOPT)" \
            OCAMLDEP="$(OCAMLDEP)" \
            OCAMLMKLIB="$(OCAMLMKLIB)" \
            nums.cmxa libnums.a && \
    cp $(OCAMLDEPS_NUM) $(abs_builddir)

all-local: nums.cmxa

clean-local: 
	cd $(srcdir)/num-1.1/src && $(MAKE) clean

