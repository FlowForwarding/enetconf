.PHONY: compile test clean

compile: rebar
	@./rebar compile

test: rebar compile
	@./rebar eunit

clean: rebar
	@./rebar clean

rebar:
	@wget -q http://cloud.github.com/downloads/basho/rebar/rebar
	@chmod u+x rebar
