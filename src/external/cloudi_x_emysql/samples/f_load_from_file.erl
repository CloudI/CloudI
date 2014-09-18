% ------------------------------------------------------------------------
% Emysql: Loading data from a file, a minimal sample
% H. Diedrich <hd2010@eonblast.com> - Eonblast http://www.eonblast.com
% 23 Apr 2013
% Thanks to Qing Liang <https://github.com/qingliangcn>
% ------------------------------------------------------------------------
%
% Re INFILE see MySQL: http://dev.mysql.com/doc/refman/5.1/de/load-data.html
%
% Create local mysql database (or re-use the one made for a_hello):
%
% $ mysql ...
% mysql> create database hello_database;
% mysql> use hello_database;
% mysql> create table hello_table (hello_text char(20));
% mysql> grant all privileges on hello_database.* to hello_username@localhost identified by 'hello_password';
% mysql> grant file on *.* to hello_username@localhost identified by 'hello_password';
% mysql> quit
%
% If you re-use the database from previous examples, be sure to add the global
% FILE privilege as in the forlast mysql> line above!
%
% On *nix build and run using the batch f_load_from_file in folder samples/:
%
% $ ./f_load_from_file
%
% - or -
%
% Make emysql and start this sample directly, along these lines:
%
% $ cd Emysql
% $ make
% $ cd samples
% $ erlc f_load_from_file.erl
% $ erl -pa ../ebin -s f_load_from_file run -s init stop -noshell
%
% ------------------------------------------------------------------------
%
% Expected Output: (note the result being a list, and the final ok_package)
%
%   Connect
%   -------
%
%   Load Record From File f_hello.txt
%   ---------------------------------
%
%   Test Select
%   -----------
%   {result_packet,5,
%                  [{field,2,<<"def">>,<<"hello_database">>,<<"hello_table">>,
%                          <<"hello_table">>,<<"hello_text">>,<<"hello_text">>,
%                          254,<<>>,33,60,0,0}],
%                  [[<<"Hello, world!">>]],
%                  <<>>}
%
% ------------------------------------------------------------------------

-module(f_load_from_file).
-export([run/0]).

run() ->

	crypto:start(),
	application:start(emysql),

	io:format("~nConnect~n-------~n", []),
	emysql:add_pool(hello_pool, 1,
		"hello_username", "hello_password", "localhost", 3306,
		"hello_database", utf8),

	io:format("~nClear Table~n-----------~n", []),
	emysql:execute(hello_pool,
		<<"DELETE FROM hello_table">>),

    {ok, Dir} = file:get_cwd(),

	io:format("~nLoad Record From File f_hello.txt" ++
	          "~n---------------------------------~n", []),
	Result = emysql:execute(hello_pool,
	    list_to_binary("LOAD DATA INFILE '" ++ Dir ++
	        "/f_hello.txt' INTO TABLE hello_table (hello_text)")),

    case Result of
        {ok_packet,_,_,_,_,_,_} ->
            ok;
        _ ->
            io:format("~n~p~n", [Result]),
            io:format("~nNote: you need to grant: " ++
                "grant file on *.* to hello_username@localhost identified by" ++
                "'hello_password';~n")
    end,

	io:format("~nTest Select~n-----------~n", []),
	Result1 = emysql:execute(hello_pool,
  	<<"select * from hello_table limit 1">>),
	io:format("~p~n", [Result1]),

	done.
