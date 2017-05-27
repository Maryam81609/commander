REBAR = $(shell pwd)/rebar3

all: compile rel

compile:
	$(REBAR) compile

rel:
	$(REBAR) release

distclean: clean relclean
	$(REBAR) clean --all

clean:
	$(REBAR) do clean

relclean:
	rm -rf _build/default/rel


console: rel
	./_build/default/rel/commander/bin/commander console

run-test: rel
	rm -f test/*.beam
	rm -f comm_tests/*.beam
	erlc -o comm_tests/ comm_tests/$(TEST)_comm.erl
	mkdir -p logs
	ct_run -pa ./_build/default/lib/*/ebin -logdir logs -suite test/commander_SUITE.er