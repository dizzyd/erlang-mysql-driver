%% file: mysql_test.erl
%% author: Yariv Sadan (yarivvv@gmail.com)
%% for license see COPYING
%% improved by ZhengXujin (xujinzheng@gmail.com)

%% 
%% mysql script:
%% delimiter $$
%% 
%% CREATE DATABASE `erlang_mysql_driver` /*!40100 DEFAULT CHARACTER SET utf8 */$$
%% 
%% CREATE TABLE `developer` (
%%   `name` varchar(255) NOT NULL,
%%   `country` varchar(255) DEFAULT NULL,
%%   PRIMARY KEY (`name`)
%% ) ENGINE=InnoDB DEFAULT CHARSET=utf8$$
%% 

-module(mysql_tests).
-include_lib("eunit/include/eunit.hrl").

-define(DB, "erlang_mysql_driver").
 
mysql_test() ->  
    %% Start the MySQL dispatcher and create the first connection
    %% to the database. 'p1' is the connection pool identifier.
    mysql:start_link(p1, "localhost", "root", "", ?DB),

    %% Add 2 more connections to the connection pool
    mysql:connect(p1, "localhost", undefined, "root", "", ?DB,
		  true),
    mysql:connect(p1, "localhost", undefined, "root", "", ?DB,
		  true),
    
    mysql:fetch(p1, <<"DELETE FROM developer">>),

    mysql:fetch(p1, <<"INSERT INTO developer(name, country) VALUES "
		     "('Claes (Klacke) Wikstrom', 'Sweden'),"
		     "('Ulf Wiger', 'USA')">>),

    %% Execute a query (using a binary)
    Result1 = mysql:fetch(p1, <<"SELECT * FROM developer">>),
    io:format("Result1: ~p~n", [Result1]),

    Except1 = {data,{mysql_result,[{<<"developer">>,<<"name">>,255,'VAR_STRING'},
                     {<<"developer">>,<<"country">>,255,'VAR_STRING'}],
                      [[<<"Claes (Klacke) Wikstrom">>,<<"Sweden">>],
                       [<<"Ulf Wiger">>,<<"USA">>]],
                    0,0,[],0,[]}},

    ?assertEqual(Except1, Result1),
    
    %% Register a prepared statement
    mysql:prepare(update_developer_country,
		  <<"UPDATE developer SET country=? where name like ?">>),
    
    %% Execute the prepared statement
    mysql:execute(p1, update_developer_country, [<<"Sweden">>, <<"%Wiger">>]),
    
    Result2 = mysql:fetch(p1, <<"SELECT * FROM developer">>),

    Except2 = {data,{mysql_result,[{<<"developer">>,<<"name">>,255,'VAR_STRING'},
                              {<<"developer">>,<<"country">>,255,
                               'VAR_STRING'}],
                             [[<<"Claes (Klacke) Wikstrom">>,<<"Sweden">>],
                              [<<"Ulf Wiger">>,<<"Sweden">>]],
                             0,0,[],0,[]}},
    
    ?assertEqual(Except2, Result2),

    mysql:transaction(
      p1,
      fun() -> mysql:fetch(<<"INSERT INTO developer(name, country) VALUES "
			    "('Joe Armstrong', 'USA')">>),
	       mysql:fetch(<<"DELETE FROM developer WHERE name like "
			    "'Claes%'">>)
      end),

    Result3 = mysql:fetch(p1, <<"SELECT * FROM developer">>),

    Except3 = {data,{mysql_result,[{<<"developer">>,<<"name">>,255,'VAR_STRING'},
                              {<<"developer">>,<<"country">>,255,
                               'VAR_STRING'}],
                             [[<<"Joe Armstrong">>,<<"USA">>],
                              [<<"Ulf Wiger">>,<<"Sweden">>]],
                             0,0,[],0,[]}},

    ?assertEqual(Except3, Result3),

    mysql:prepare(delete_all, <<"DELETE FROM developer">>),

    {aborted, {{error, foo}, {rollback_result, _RollCmd}}} = mysql:transaction(
		     p1,
		     fun() -> mysql:execute(delete_all),
			      throw({error, foo})
		     end),

    Result4 = mysql:fetch(p1, <<"SELECT * FROM developer">>),
    ?assertEqual(Except3, Result4).