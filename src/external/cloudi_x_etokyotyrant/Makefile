APP_NAME="medici"
VSN="0.5"

all: compile

docs: 
	erl -noshell -run edoc_run application "'$(APP_NAME)'" '"."' '$(VSN)'

compile: clean
	erlc -o ebin/ src/*.erl
	cp src/*.app ebin/

clean:
	rm -rfv ebin/
	mkdir ebin

# Testing with a Tokyo Tyrant server instance
#test:	clean ttclean tt_normal testbuild run_basic_test tt_table run_table_test tt_normal run_medici_test ttstopd ttclean

test:	clean ttclean testbuild run_basic_test run_table_test run_medici_test ttstopd

testbuild:
	erlc -DTEST -DDEBUG +debug_info -o ebin/ src/*.erl
	cp src/*.app ebin/
run_basic_test:
	ttserver -dmn -kl -pid /tmp/medici_server.pid /tmp/medici_server.tch
	erl -pa ebin/ -noshell -s principe test -s init stop
run_table_test:
	ttserver -dmn -kl -pid /tmp/medici_server.pid /tmp/medici_server.tct
	erl -pa ebin/ -noshell -s principe_table test -s init stop
run_medici_test:
	ttserver -dmn -kl -pid /tmp/medici_server.pid /tmp/medici_server.tch
	erl -pa ebin/ -noshell -s medici test -s init stop
ttclean:
	rm -f /tmp/medici_server.*
tt_normal:
	ttserver -dmn -kl -pid /tmp/medici_server.pid /tmp/medici_server.tch
tt_table:
	ttserver -dmn -kl -pid /tmp/medici_server.pid /tmp/medici_server.tct
ttstopd:
	kill -TERM `head -1 /tmp/medici_server.pid`
	rm -f /tmp/medici_server.*
