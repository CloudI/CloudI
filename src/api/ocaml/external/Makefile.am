#-*-Mode:make;coding:utf-8;tab-width:4;c-basic-offset:4-*-
# ex: set ft=make fenc=utf-8 sts=4 ts=4 sw=4 noet nomod:

CLEANFILES = nums.a nums.cmxa

all-local: 
	cd $(srcdir)/num-1.1/src && \
    $(MAKE) OCAMLC="$(OCAMLC)" \
            OCAMLOPT="$(OCAMLOPT)" \
            OCAMLDEP="$(OCAMLDEP)" \
            OCAMLMKLIB="$(OCAMLMKLIB)" \
            nums.cmxa && \
    cp nums.a nums.cmxa $(abs_builddir)

clean-local: 
	cd $(srcdir)/num-1.1/src && $(MAKE) clean

