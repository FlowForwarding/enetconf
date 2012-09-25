.PHONY: compile test run clean

PWD := $(shell pwd)
APP := $(shell basename $(PWD))

compile: rebar
	@./rebar compile

test: rebar compile
	@./rebar eunit

clean: rebar
	@./rebar clean

run: compile
	@erl -pa ../$(APP)/ebin -eval \
	 "[application:start(A) || A <- [crypto, ssh, xmerl, enetconf]]"

rebar:
	@wget -q http://cloud.github.com/downloads/basho/rebar/rebar
	@chmod u+x rebar
