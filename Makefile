all:
	(cd apps/introflection/priv ; grunt)
	rebar compile skip-deps=true
	(cd rel ; rebar generate)
	./rel/introflection/bin/introflection console

test:
	(cd apps/introflection ; erl -make)
	(cd apps/introflection ; ct_run -pa ebin/ -spec introflection.spec)
