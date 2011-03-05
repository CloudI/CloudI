ERLECTRICITY_VERSION = 1.1.1
ERLECTRICITY_DESTINATION = gems/erlectricity-$(ERLECTRICITY_VERSION)

all-local: $(ERLECTRICITY_DESTINATION)

$(ERLECTRICITY_DESTINATION): $(top_builddir)/external/erlectricity
	@PWD=`pwd`
	@(cd $(top_builddir)/external/erlectricity; \
      $(RUBY_GEM) install --install-dir $(PWD) erlectricity)

clean-local:
	rm -rf cache doc gems specifications

