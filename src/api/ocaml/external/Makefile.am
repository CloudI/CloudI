#-*-Mode:make;coding:utf-8;tab-width:4;c-basic-offset:4-*-
# ex: set ft=make fenc=utf-8 sts=4 ts=4 sw=4 noet nomod:

OCAMLDEPS_NUM = \
    arith_flags.cmi \
    arith_status.cmi \
    big_int.cmi \
    int_misc.cmi \
    nat.cmi \
    num.cmi \
    ratio.cmi \
    libnums.a \
    nums.a \
    nums.cmxa \
    nums.cma
CLEANFILES = $(OCAMLDEPS_NUM)

nums.cmxa:
	cd $(srcdir)/num-1.1/src && \
    $(MAKE) OCAMLC="$(OCAMLC)" \
            OCAMLOPT="$(OCAMLOPT)" \
            OCAMLDEP="$(OCAMLDEP)" \
            OCAMLMKLIB="$(OCAMLMKLIB)" \
            nums.cmxa nums.cma libnums.a && \
    cp $(OCAMLDEPS_NUM) $(abs_builddir)

all-local: nums.cmxa

clean-local: 
	cd $(srcdir)/num-1.1/src && $(MAKE) clean

