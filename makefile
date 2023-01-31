shell:
	ERL_AFLAGS="-enable-feature all" rebar3 shell

fmt:
	ERL_AFLAGS="-enable-feature all" rebar3 fmt

eunit:
	ERL_AFLAGS="-enable-feature all" rebar3 eunit

compile:
	rebar3 compile

build_release:
	rebar3 as prod release

release: 
	./bin/release.sh

build: compile eunit
build_release: build build_release
