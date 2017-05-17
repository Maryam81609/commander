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
	./_build/default/rel/antidote/bin/env console


systests: rel
	rm -f test/*.beam
	mkdir -p logs

ifdef SUITE
	ct_run -pa ./_build/default/lib/*/ebin -logdir logs -suite test/${SUITE}
else
	ct_run -pa ./_build/default/lib/*/ebin -logdir logs -dir test
endif