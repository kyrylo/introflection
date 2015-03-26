REBAR=$(shell which rebar || echo ./rebar)

.PHONY: all

all: deps js compile generate run

js:
	(cd priv ; grunt)

compile:
	$(REBAR) compile

deps:
	$(REBAR) get-deps

clean:
	$(REBAR) clean

generate:
	(cd rel ; $(REBAR) generate)

test:
	erl -make
	ct_run -pa ebin/ deps/*/ebin/ -spec introflection.spec

dialyzer:
	$(REBAR) skip_deps=true dialyze

run:
	./rel/introflection/bin/introflection console
