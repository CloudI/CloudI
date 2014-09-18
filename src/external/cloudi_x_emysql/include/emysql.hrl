%% Copyright (c) 2009-2012
%% Bill Warnecke <bill@rupture.com>,
%% Jacob Vorreuter <jacob.vorreuter@gmail.com>,
%% Henning Diedrich <hd2010@eonblast.com>,
%% Eonblast Corporation <http://www.eonblast.com>
%% 
%% Permission is  hereby  granted,  free of charge,  to any person
%% obtaining  a copy of this software and associated documentation
%% files (the "Software"),to deal in the Software without restric-
%% tion,  including  without  limitation the rights to use,  copy, 
%% modify, merge,  publish,  distribute,  sublicense,  and/or sell
%% copies  of the  Software,  and to  permit  persons to  whom the
%% Software  is  furnished  to do  so,  subject  to the  following 
%% conditions:
%% 
%% The above  copyright notice and this permission notice shall be
%% included in all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
%% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
%% OF  MERCHANTABILITY,  FITNESS  FOR  A  PARTICULAR  PURPOSE  AND
%% NONINFRINGEMENT. IN  NO  EVENT  SHALL  THE AUTHORS OR COPYRIGHT
%% HOLDERS  BE  LIABLE FOR  ANY CLAIM, DAMAGES OR OTHER LIABILITY,
%% WHETHER IN AN ACTION OF CONTRACT,  TORT  OR OTHERWISE,  ARISING
%% FROM,  OUT OF OR IN CONNECTION WITH THE SOFTWARE  OR THE USE OR
%% OTHER DEALINGS IN THE SOFTWARE.


-record(pool, {pool_id, size, user, password, host, port, database, encoding, available=queue:new(), locked=gb_trees:empty(), waiting=queue:new(), start_cmds=[], conn_test_period=0, connect_timeout=infinity}).
-record(emysql_connection, {id, pool_id, encoding, socket, version, thread_id, caps, language, prepared=gb_trees:empty(), locked_at, alive=true, test_period=0, last_test_time=0, monitor_ref}).
-record(greeting, {protocol_version, server_version, thread_id, salt1, salt2, caps, caps_high, language, status, seq_num, plugin}).
-record(field, {seq_num, catalog, db, table, org_table, name, org_name, type, default, charset_nr, length, flags, decimals, decoder}).
-record(packet, {size, seq_num, data}).
-record(ok_packet, {seq_num, affected_rows, insert_id, status, warning_count, msg}).
-record(error_packet, {seq_num, code, status, msg}).
-record(eof_packet, {seq_num, status, warning_count}). % extended to mySQL 4.1+ format
-record(result_packet, {seq_num, field_list, rows, extra}).

-define(TIMEOUT, 8000).
-define(LOCK_TIMEOUT, 5000).
-define(MAXPACKETBYTES, 50000000).
-define(LONG_PASSWORD, 1).
-define(LONG_FLAG, 4).
-define(CLIENT_LOCAL_FILE, 128).
-define(PROTOCOL_41, 512).
-define(CLIENT_MULTI_STATEMENTS, 65536).
-define(CLIENT_MULTI_RESULTS, 131072).
-define(TRANSACTIONS, 8192).
-define(SECURE_CONNECTION, 32768).
-define(CONNECT_WITH_DB, 8).
-define(CONN_TEST_PERIOD, 28000).
-define(TCP_RECV_BUFFER, 8192).

%% MYSQL COMMANDS
-define(COM_SLEEP, 16#00).
-define(COM_QUIT, 16#01).
-define(COM_INIT_DB, 16#02).
-define(COM_QUERY, 16#03).
-define(COM_FIELD_LIST, 16#04).
-define(COM_CREATE_DB, 16#05).
-define(COM_DROP_DB, 16#06).
-define(COM_REFRESH, 16#07).
-define(COM_SHUTDOWN, 16#08).
-define(COM_STATISTICS, 16#09).
-define(COM_PROCESS_INFO, 16#0a).
-define(COM_CONNECT, 16#0b).
-define(COM_PROCESS_KILL, 16#0c).
-define(COM_DEBUG, 16#0d).
-define(COM_PING, 16#0e).
-define(COM_TIME, 16#0f).
-define(COM_DELAYED_INSERT, 16#10).
-define(COM_CHANGE_USER, 16#11).
-define(COM_BINLOG_DUMP, 16#12).
-define(COM_TABLE_DUMP, 16#13).
-define(COM_CONNECT_OUT, 16#14).
-define(COM_REGISTER_SLAVE, 16#15).
-define(COM_STMT_PREPARE, 16#16).
-define(COM_STMT_EXECUTE, 16#17).
-define(COM_STMT_SEND_LONG_DATA, 16#18).
-define(COM_STMT_CLOSE, 16#19).
-define(COM_STMT_RESET, 16#1a).
-define(COM_SET_OPTION, 16#1b).
-define(COM_STMT_FETCH, 16#1c).

%% MYSQL TYPES
-define(FIELD_TYPE_DECIMAL, 16#00).
-define(FIELD_TYPE_TINY, 16#01).
-define(FIELD_TYPE_SHORT, 16#02).
-define(FIELD_TYPE_LONG, 16#03).
-define(FIELD_TYPE_FLOAT, 16#04).
-define(FIELD_TYPE_DOUBLE, 16#05).
-define(FIELD_TYPE_NULL, 16#06).
-define(FIELD_TYPE_TIMESTAMP, 16#07).
-define(FIELD_TYPE_LONGLONG, 16#08).
-define(FIELD_TYPE_INT24, 16#09).
-define(FIELD_TYPE_DATE, 16#0a).
-define(FIELD_TYPE_TIME, 16#0b).
-define(FIELD_TYPE_DATETIME, 16#0c).
-define(FIELD_TYPE_YEAR, 16#0d).
-define(FIELD_TYPE_NEWDATE, 16#0e).
-define(FIELD_TYPE_VARCHAR, 16#0f).
-define(FIELD_TYPE_BIT, 16#10).
-define(FIELD_TYPE_NEWDECIMAL, 16#f6).
-define(FIELD_TYPE_ENUM, 16#f7).
-define(FIELD_TYPE_SET, 16#f8).
-define(FIELD_TYPE_TINY_BLOB, 16#f9).
-define(FIELD_TYPE_MEDIUM_BLOB, 16#fa).
-define(FIELD_TYPE_LONG_BLOB, 16#fb).
-define(FIELD_TYPE_BLOB, 16#fc).
-define(FIELD_TYPE_VAR_STRING, 16#fd).
-define(FIELD_TYPE_STRING, 16#fe).
-define(FIELD_TYPE_GEOMETRY, 16#ff).

%% MSQL SERVER STATES (mysql_com.h)
-define(SERVER_NO_STATUS, 0).
-define(SERVER_STATUS_IN_TRANS, 1).	% Transaction has started */
-define(SERVER_STATUS_AUTOCOMMIT, 2). % Server in auto_commit mode */
-define(SERVER_MORE_RESULTS_EXIST, 8). % Multi query - next query exists */
-define(SERVER_QUERY_NO_GOOD_INDEX_USED, 16).
-define(SERVER_QUERY_NO_INDEX_USED, 32).

%  The server was able to fulfill the clients request and opened a
%  read-only non-scrollable cursor for a query. This flag comes
%  in reply to COM_STMT_EXECUTE and COM_STMT_FETCH commands.
-define(SERVER_STATUS_CURSOR_EXISTS, 64).

%  This flag is sent when a read-only cursor is exhausted, in reply to
%  COM_STMT_FETCH command.
-define(SERVER_STATUS_LAST_ROW_SENT, 128).
-define(SERVER_STATUS_DB_DROPPED, 256). % A database was dropped
-define(SERVER_STATUS_NO_BACKSLASH_ESCAPES, 512).

%  Sent to the client if after a prepared statement reprepare
%  we discovered that the new statement returns a different
%  number of result set columns.
-define(SERVER_STATUS_METADATA_CHANGED, 1024).

%% RESPONSE
-define(RESP_OK, 0).
-define(RESP_EOF, 254).
-define(RESP_ERROR, 255).

%% AUTH PLUGIN
-define(MYSQL_OLD_PASSWORD, <<"mysql_old_password">>).
