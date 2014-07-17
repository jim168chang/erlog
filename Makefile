all:
	rebar get-deps clean compile

build:
	rebar clean compile

run: build
	@erl -noshell -pa './ebin' -eval "application:start(erlog)"

run_shell: build
	@erl -pa './ebin' -eval "application:start(erlog)"

clean:
	rebar clean

test: build
	@mkdir ebin/tests
	@erlc -pa './ebin' -o 'ebin/tests' tests/*.erl
	@erl -noshell -pa './ebin' -pa './ebin/tests' -s erlog_tests tests -s init stop
