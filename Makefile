#
# run rebar and do some supporting stuff
#
NODENAMES=server_a server_b

.PHONY: deps doc

all: deps compile
#all: compile unit-test dist-test

compile:
	./rebar compile

deps:
	./rebar get-deps

clean:
	./rebar clean

distclean: clean
	./rebar delete-deps

dialyzer: compile
	@dialyzer -Wno_return -c ebin

doc :
	@./rebar doc skip_deps=true

unit-test:
	ERL_FLAGS="-name rebar" ./rebar eunit

dist-test:
#	for name in ${NODENAMES} ; do erl_call -s -name $$name ; done
	echo "q()." | erl -name ct -run ct_master run docterl_ets.test.spec
#	for name in ${NODENAMES} ; do erl_call -q -name $$name ; done
