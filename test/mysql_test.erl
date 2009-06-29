%% file: mysql_test.erl
%% author: Yariv Sadan (yarivvv@gmail.com)
%% for license see COPYING

-module(mysql_test).
-compile(export_all).

test() ->
    compile:file("/usr/local/lib/erlang/lib/mysql/mysql.erl"),
    compile:file("/usr/local/lib/erlang/lib/mysql/mysql_conn.erl"),
    
    %% Start the MySQL dispatcher and create the first connection
    %% to the database. 'p1' is the connection pool identifier.
    mysql:start_link(p1, "localhost", "root", "password", "test"),

    %% Add 2 more connections to the connection pool
    mysql:connect(p1, "localhost", undefined, "root", "password", "test",
		  true),
    mysql:connect(p1, "localhost", undefined, "root", "password", "test",
		  true),
    
    mysql:fetch(p1, <<"DELETE FROM developer">>),

    mysql:fetch(p1, <<"INSERT INTO developer(name, country) VALUES "
		     "('Claes (Klacke) Wikstrom', 'Sweden'),"
		     "('Ulf Wiger', 'USA')">>),

    %% Execute a query (using a binary)
    Result1 = mysql:fetch(p1, <<"SELECT * FROM developer">>),
    io:format("Result1: ~p~n", [Result1]),
    
    %% Register a prepared statement
    mysql:prepare(update_developer_country,
		  <<"UPDATE developer SET country=? where name like ?">>),
    
    %% Execute the prepared statement
    mysql:execute(p1, update_developer_country, [<<"Sweden">>, <<"%Wiger">>]),
    
    Result2 = mysql:fetch(p1, <<"SELECT * FROM developer">>),
    io:format("Result2: ~p~n", [Result2]),
    
    mysql:transaction(
      p1,
      fun() -> mysql:fetch(<<"INSERT INTO developer(name, country) VALUES "
			    "('Joe Armstrong', 'USA')">>),
	       mysql:fetch(<<"DELETE FROM developer WHERE name like "
			    "'Claes%'">>)
      end),

    Result3 = mysql:fetch(p1, <<"SELECT * FROM developer">>),
    io:format("Result3: ~p~n", [Result3]),
    
    mysql:prepare(delete_all, <<"DELETE FROM developer">>),

    {error, foo} = mysql:transaction(
		     p1,
		     fun() -> mysql:execute(delete_all),
			      throw({error, foo})
		     end),

    Result4 = mysql:fetch(p1, <<"SELECT * FROM developer">>),
    io:format("Result4: ~p~n", [Result4]),
				    
    ok.
    
    
