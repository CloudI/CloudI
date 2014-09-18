LIBDIR=$(shell erl -eval 'io:format("~s~n", [code:lib_dir()])' -s init stop -noshell)
VERSION=0.4.1
PKGNAME=emysql
APP_NAME=emysql

MODULES=$(shell ls -1 src/*.erl | awk -F[/.] '{ print $$2 }' | sed '$$q;s/$$/,/g')
MAKETIME=$(shell date)

all: crypto_compat app
	(cd src;$(MAKE))

app: ebin/$(PKGNAME).app

crypto_compat:
	(escript support/crypto_compat.escript)

ebin/$(PKGNAME).app: src/$(PKGNAME).app.src
	mkdir -p ebin
	sed -e 's/modules, \[\]/modules, [$(MODULES)]/;s/%MAKETIME%/$(MAKETIME)/' < $< > $@

# Create doc HTML from source comments
docs:
	erl -noshell -run edoc_run application "'emysql'" '"."' '[{def,{vsn,""}},{stylesheet, "emysql-style.css"}]'

# Pushes created docs into dir ../Emysql-github-pages to push to github pages.
# Make sure to do 'make docs' first.
# will fail if you haven't checked out github pages into ../Emysql-github-pages
pages:
	(cd ../Emysql-github-pages; git pull origin gh-pages)
	cp -r doc/* ../Emysql-github-pages
	(cd ../Emysql-github-pages; git add .; git commit -m 'make pages'; git push origin gh-pages)

# Create HTML from Markdown to test README.md appearance
markdown:
	lua etc/markdown.lua README.md

hello:
	erlc hello.erl
	erl -pa ./ebin -s hello run -s init stop -noshell

clean:
	(cd src;$(MAKE) clean)
	(cd t;$(MAKE) clean)
	rm -rf ebin/*.app cover erl_crash.dump
	rm -f ebin/erl_crash.dump
	rm -f src/erl_crash.dump
	rm -f erl_crash.dump
	rm -f doc/*.html
	rm -rf test/ct_run*
	rm -f test/variables-ct*
	rm -f test/*.beam
	rm -f test/*.html
	rm -rf ct_run*
	rm -f variables-ct*
	rm -f *.beam
	rm -f *.html
	rm -f include/crypto_compat.hrl
	rm -fr logs

package: clean
	@mkdir $(PKGNAME)-$(VERSION)/ && cp -rf ebin include Makefile README src support t $(PKGNAME)-$(VERSION)
	@COPYFILE_DISABLE=true tar zcf $(PKGNAME)-$(VERSION).tgz $(PKGNAME)-$(VERSION)
	@rm -rf $(PKGNAME)-$(VERSION)/

install:
	@for i in ebin/*.beam ebin/*.app include/*.hrl src/*.erl; do install -m 644 -D $$i $(prefix)/$(LIBDIR)/$(PKGNAME)-$(VERSION)/$$i ; done

all-test: test testutil

CT_OPTS ?=
CT_RUN = ct_run \
        -no_auto_compile \
        -noshell \
        -pa $(realpath ebin) \
        -dir test \
        -logdir logs \
        -cover test/cover.spec -cover_stop false \
        $(CT_OPTS)
# Currently, the order of the test cases matter!
CT_SUITES=environment basics conn_mgr  emysql_util

build-tests:
	erlc -v -o test/ $(wildcard test/*.erl) -pa ebin/

test: all build-tests
	@mkdir -p logs
	$(CT_RUN) -suite $(addsuffix _SUITE,$(CT_SUITES)) ; \

prove: all
	(cd t;$(MAKE))
	prove t/*.t

APPS = kernel stdlib erts crypto public_key ssl compiler asn1
REPO = emysql
COMBO_PLT = $(HOME)/.$(REPO)_combo_dialyzer_plt
build_plt:
	dialyzer --build_plt --output_plt $(COMBO_PLT) --apps $(APPS)

dialyzer:
	dialyzer --fullpath -nn --plt $(COMBO_PLT) ebin
