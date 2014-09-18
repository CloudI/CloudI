% ------------------------------------------------------------------------
% Emysql: Stored procedure, a minimal sample
% H. Diedrich <hd2010@eonblast.com> - Eonblast http://www.eonblast.com
% 12 Jun 2010
% Thanks to Seven Du <https://github.com/seven1240>
% ------------------------------------------------------------------------
%
% Create local mysql database (or re-use the one made for a_hello):
%
% $ mysql ...
% mysql> create database hello_database;
% mysql> use hello_database;
% mysql> create table hello_table (hello_text char(20));
% mysql> grant all privileges on hello_database.* to hello_username@localhost identified by 'hello_password';
% mysql> quit
%
% On *nix build and run using the batch e_stored_procedure in folder samples/:
%
% $ ./e_stored_procedure
%
% - or - 
%
% Make emysql and start this sample directly, along these lines:
%
% $ cd Emysql
% $ make
% $ cd samples
% $ erlc e_stored_procedure.erl
% $ erl -pa ../ebin -s e_stored_procedure run -s init stop -noshell
%
% ------------------------------------------------------------------------
%
% Expected Output: (note the result being a list, and the final ok_package)
%
%   Connect
%   -------
%   
%   Create Record
%   -------------
%   
%   Test Select
%   -----------
%   {result_packet,6,
%                  [{field,2,<<"def">>,<<"hello_database">>,<<"hello_table">>,
%                          <<"hello_table">>,<<"hello_text">>,<<"hello_text">>,
%                          254,<<>>,33,60,0,0}],
%                  [[<<"Hello World!">>],[<<"Hello World!">>]],
%                  <<>>}
%   
%   Create Stored Procedure
%   -----------------------
%   {ok_packet,1,0,0,2,0,[]}
%   
%   Call Stored Procedure
%   ---------------------
%   [{result_packet,6,
%                   [{field,2,<<"def">>,<<"hello_database">>,<<"hello_table">>,
%                           <<"hello_table">>,<<"hello_text">>,<<"hello_text">>,
%                           254,<<>>,33,60,0,0}],
%                   [[<<"Hello World!">>],[<<"Hello World!">>]],
%                   <<>>},
%    {ok_packet,7,0,0,34,0,[]}]
%   %   
% ------------------------------------------------------------------------

-module(e_stored_procedure).
-export([run/0]).

run() ->

	crypto:start(),
	application:start(emysql),

	io:format("~nConnect~n-------~n", []),
	emysql:add_pool(hello_pool, 1,
		"hello_username", "hello_password", "localhost", 3306,
		"hello_database", utf8),

	io:format("~nCreate Record~n-------------~n", []),
	emysql:execute(hello_pool,
		<<"INSERT INTO hello_table SET hello_text = 'Hello World!'">>),

	io:format("~nTest Select~n-----------~n", []), 
	Result1 = emysql:execute(hello_pool,
  	<<"select * from hello_table limit 2">>),
	io:format("~p~n", [Result1]),

	io:format("~nCreate Stored Procedure~n-----------------------~n", []),
	Result2 = emysql:execute(hello_pool,
  	<<"create procedure sp_hello() begin select * from hello_table limit 2; end">>),
	io:format("~p~n", [Result2]),

	io:format("~nCall Stored Procedure~n---------------------~n", []),
	Result3 = emysql:execute(hello_pool,
	   	<<"call sp_hello();">>),
		io:format("~p~n", [Result3]),
	
	done.
