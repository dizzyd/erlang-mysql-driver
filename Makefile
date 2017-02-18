all:
	./rebar compile

clean:
	./rebar clean

run:
	erl -pa ./ebin
