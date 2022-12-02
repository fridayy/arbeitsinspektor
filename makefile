shell:
	ERL_AFLAGS="-enable-feature all" rebar3 shell

fmt:
	ERL_AFLAGS="-enable-feature all" rebar3 fmt

eunit:
	ERL_AFLAGS="-enable-feature all" rebar3 eunit

compile:
	rebar3 compile

build: compile eunit
