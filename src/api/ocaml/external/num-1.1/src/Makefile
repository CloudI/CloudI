OCAMLC ?= ocamlc
OCAMLOPT ?= ocamlopt
OCAMLDEP ?= ocamldep
OCAMLMKLIB ?= ocamlmklib
OCAMLFIND=ocamlfind
INSTALL_DATA=install -m 644
INSTALL_DLL=install
STDLIBDIR=$(shell $(OCAMLC) -where)

include $(STDLIBDIR)/Makefile.config

ifeq "$(ARCH)" "i386"
BNG_ARCH=ia32
else ifeq "$(ARCH)" "amd64"
BNG_ARCH=amd64
else ifeq "$(ARCH)" "arm64"
BNG_ARCH=arm64
else ifeq "$(ARCH)" "power"
BNG_ARCH=ppc
else
BNG_ARCH=generic
endif

CAMLCFLAGS=-w +a-4-9-41-42-44-45-48 -warn-error A -bin-annot -g \
          -safe-string -strict-sequence -strict-formats
CAMLOPTFLAGS=$(CAMLCFLAGS)
ifeq "$(FLAMBDA)" "true"
CAMLOPTFLAGS+=-O3
endif

CMIS=big_int.cmi nat.cmi num.cmi ratio.cmi arith_status.cmi
CMOS=int_misc.cmo nat.cmo big_int.cmo arith_flags.cmo \
  ratio.cmo num.cmo arith_status.cmo
CMXS=$(CMOS:.cmo=.cmx)
COBJS=bng.$(O) nat_stubs.$(O)

all:: libnums.$(A) nums.cma

ifneq "$(ARCH)" "none"
all:: nums.cmxa
endif

ifeq "$(NATDYNLINK)" "true"
all:: nums.cmxs
endif

libnums.$(A): $(COBJS)
	$(OCAMLMKLIB) -oc nums $(COBJS)

nums.cma: $(CMOS)
	$(OCAMLMKLIB) -o nums -oc nums -linkall $(CMOS)

nums.cmxa: $(CMXS)
	$(OCAMLMKLIB) -o nums -oc nums -linkall $(CMXS)

nums.cmxs: nums.cmxa libnums.$(A)
	$(OCAMLOPT) $(CAMLOPTFLAGS) -I . -shared -o nums.cmxs nums.cmxa

# We hard-code the C dependencies rather than having them generated
# because the dependencies are so simple.
bng.$(O): bng.h bng_*.c
nat_stubs.$(O): bng.h nat.h

%.cmi: %.mli
	$(OCAMLC) $(CAMLCFLAGS) -c $*.mli
%.cmo: %.ml
	$(OCAMLC) $(CAMLCFLAGS) -c $*.ml
%.cmx: %.ml
	$(OCAMLOPT) $(CAMLOPTFLAGS) -c $*.ml
%.$(O): %.c
	$(OCAMLC) -ccopt -DBNG_ARCH_$(BNG_ARCH) -c $*.c

# Legacy installation: files go into OCaml's stdlib directory; only META
# is installed via findlib

TOINSTALL=nums.cma libnums.$(A) $(CMIS) $(CMIS:.cmi=.mli) $(CMIS:.cmi=.cmti)
ifneq "$(ARCH)" "none"
TOINSTALL+=nums.cmxa nums.$(A) $(CMIS:.cmi=.cmx)
endif
ifeq "$(NATDYNLINK)" "true"
TOINSTALL+=nums.cmxs
endif
TOINSTALL_STUBS=dllnums.$(SO)

install:
	$(OCAMLFIND) install num META
	$(INSTALL_DATA) $(TOINSTALL) $(STDLIBDIR)
ifeq "$(SUPPORTS_SHARED_LIBRARIES)" "true"
	$(INSTALL_DLL) $(TOINSTALL_STUBS) $(STDLIBDIR)/stublibs
endif

uninstall:
	cd $(STDLIBDIR) && rm -f $(TOINSTALL)
ifeq "$(SUPPORTS_SHARED_LIBRARIES)" "true"
	cd $(STDLIBDIR)/stublibs && rm -f $(TOINSTALL_STUBS) 
endif
	$(OCAMLFIND) remove num

clean:
	rm -f *.cm[ioxta] *.cmx[as] *.cmti *.$(O) *.$(A) *.$(SO)

depend:
	$(OCAMLDEP) -slash *.mli *.ml > .depend

include .depend
