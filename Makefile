all:
	(cd apps/introflection/priv ; grunt)
	rebar compile skip-deps=true
	(cd rel ; rebar generate)
	./rel/introflection/bin/introflection console
