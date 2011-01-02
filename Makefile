all:
	./rebar compile

clean:
	./rebar clean

doc:
	erl -eval 'edoc:application (mysql_driver, ".", [])' -noshell -s init stop
