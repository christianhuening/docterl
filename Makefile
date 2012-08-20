#
# run rebar and do some supporting stuff
#
NODENAMES=server_a server_b

#all: compile unit-test dist-test
all: dist-test

compile:
	./rebar compile

unit-test:
	./rebar eunit

dist-test:
#	for name in ${NODENAMES} ; do erl_call -s -name $$name ; done
	echo "q()." | erl -name ct -run ct_master run docterl_ets.test.spec
#	for name in ${NODENAMES} ; do erl_call -q -name $$name ; done
