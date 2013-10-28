.PHONY: compile test run clean

PWD := $(shell pwd)
APP := $(shell basename $(PWD))

compile: rebar
	@./rebar compile

test: rebar compile
	@./rebar eunit
	@rm -f .eunit/enetconf

clean: rebar
	@./rebar clean
	@rm -f .eunit/enetconf

run_server: compile
	@erl -pa ../$(APP)/ebin -eval \
	 "[ok = application:start(A) || A <- [crypto, asn1, public_key, ssh, xmerl, enetconf]]"

run_client: compile
	@erl -pa ../$(APP)/ebin -eval \
	 "[ok = application:start(A) || A <- [crypto, asn1, public_key, ssh, xmerl]]"

rebar:
	@wget -q http://cloud.github.com/downloads/basho/rebar/rebar
	@chmod u+x rebar
