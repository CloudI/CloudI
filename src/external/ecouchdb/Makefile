all:
	mkdir -p ebin/
	(cd src;$(MAKE))

test: all
	prove t/*.t

clean:
	(cd src;$(MAKE) clean)
	rm -rf erl_crash.dump *.beam *.hrl cover

dist-src: clean
	tar zcvf erlang_couchdb-0.2.3.tgz Makefile src/

cover: all
	COVER=1 prove t/*.t
	erl -detached -noshell -eval 'etap_report:create()' -s init stop
