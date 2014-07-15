all: 
	rebar compile
	#@erl -noshell -pa './deps/bitcask/ebin' -pa './ebin' -s sample start

build:
	rebar clean compile

run:
	@erl -noshell -pa './ebin' -s media_uploader start

clean:
	rebar clean

test: build
	@mkdir ebin/tests
	@erlc -pa './ebin' -o 'ebin/tests' tests/*.erl
	@erl -noshell -pa './ebin' -pa './ebin/tests' -s erlog_tests tests -s init stop
