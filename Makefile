all: compile xref eunit                                                   

init:
	@rebar get-deps compile	

compile:
	@rebar compile skip_deps=true

xref:
	@rebar xref skip_deps=true

clean:
	@rebar clean skip_deps=true

eunit:
	@rebar eunit skip_deps=true

edoc:
	@rebar doc skip_deps=true

start: compile
	erl -pz ebin deps/*/ebin

dialyzer-init:
	dialyzer --build_plt --apps erts kernel stdlib -r ebin ebin deps/*/ebin

dialyzer:
	dialyzer --src -r src/
