APP=rtmpmsg
DIALYZER_OPTS=-Werror_handling -Wrace_conditions -Wunmatched_returns
LIBS=$(ERL_LIBS):deps

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
	@ERL_LIBS=$(LIBS) erl +stbt db +K true -pz ebin -eval 'erlang:display(application:ensure_all_started($(APP))).'

.dialyzer.plt:
	touch .dialyzer.plt
	ERL_LIBS=$(LIBS) dialyzer --build_plt --plt .dialyzer.plt --apps erts \
	$(shell ERL_LIBS=$(LIBS) erl -noshell -pa ebin -eval '{ok, _} = application:ensure_all_started($(APP)), [erlang:display(Name) || {Name, _, _} <- application:which_applications(), Name =/= $(APP)], halt().')

dialyze: .dialyzer.plt compile
	ERL_LIBS=$(LIBS) dialyzer -pa ebin --plt .dialyzer.plt -I deps -r ebin $(DIALYZER_OPTS)
