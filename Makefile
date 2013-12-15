all: compile xref eunit                                                   

init:
	@./rebar get-deps compile	

compile:
	@./rebar compile skip_deps=true

xref:
	@./rebar xref skip_deps=true

clean:
	@./rebar clean skip_deps=true

eunit:
	@./rebar eunit skip_deps=true

edoc:
	@./rebar doc skip_deps=true

start: compile
	erl -pz ebin deps/*/ebin

.dialyzer.plt:
	touch .dialyzer.plt
	dialyzer --build_plt --plt .dialyzer.plt --apps erts kernel stdlib -r ebin deps/*/ebin

dialyze: .dialyzer.plt
	dialyzer --plt .dialyzer.plt -r ebin
