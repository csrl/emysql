%% Copyright (c) 2009-2012
%% Bill Warnecke <bill@rupture.com>
%% Jacob Vorreuter <jacob.vorreuter@gmail.com>
%% Henning Diedrich <hd2010@eonblast.com>
%%
%% Copyright (c) 2013
%%
%% Permission is hereby granted, free of charge, to any person
%% obtaining a copy of this software and associated documentation
%% files (the "Software"), to deal in the Software without
%% restriction, including without limitation the rights to use,
%% copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the
%% Software is furnished to do so, subject to the following
%% conditions:
%%
%% The above copyright notice and this permission notice shall be
%% included in all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
%% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
%% OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
%% NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
%% HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
%% WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
%% OTHER DEALINGS IN THE SOFTWARE.
%%==============================================================================

%% Pool/Connection
-record(pool, {
  pool_id, config, size = 0,
  available=queue:new(), locked=gb_trees:empty(), waiting=queue:new()}).
-record(pool_config, {
  user, password, host, port, database, collation}).
-record(connection, {
  socket, pool_id, thread_id, locked_at}).
-record(greeting, {
  seq_num, protocol_version, server_version, thread_id, salt, caps, collation,
  status, plugin}).
-record(authswitch, {
  seq_num}).

%% Records returned externally
-record(field, {
  seq_num, db, table, org_table, name, org_name, type, default,
  charset_nr, length, flags, decimals}).
-record(ok_packet, {
  seq_num, affected_rows, insert_id, status, warning_count, msg}).
-record(error_packet, {
  seq_num, code, status, msg}).
-record(result_packet, {
  seq_num, fields, rows}).

%% MYSQL CLIENT/SERVER CAPABILITIES
-define(CLIENT_LONG_PASSWORD, 1).
-define(CLIENT_FOUND_ROWS, 2).                 %% Not Implemented
-define(CLIENT_LONG_FLAG, 4).
-define(CLIENT_CONNECT_WITH_DB, 8).
-define(CLIENT_NO_SCHEMA, 16).                 %% Not Implemented
-define(CLIENT_COMPRESS, 32).                  %% Not Implemented
-define(CLIENT_ODBC, 64).                      %% Obsolete
-define(CLIENT_LOCAL_FILES, 128).              %% Not Implemented
-define(CLIENT_IGNORE_SPACE, 256).             %% Not Implemented
-define(CLIENT_PROTOCOL_41, 512).
-define(CLIENT_INTERACTIVE, 1024).             %% Not Implemented
-define(CLIENT_SSL, 2048).                     %% Not Implemented
-define(CLIENT_IGNORE_SIGPIPE, 4096).          %% Not Applicable
-define(CLIENT_TRANSACTIONS, 8192).
-define(CLIENT_RESERVED, 16384).               %% Obsolete
-define(CLIENT_SECURE_CONNECTION, 32768).
-define(CLIENT_MULTI_STATEMENTS, 65536).
-define(CLIENT_MULTI_RESULTS, 131072).
-define(CLIENT_PS_MULTI_RESULTS, 262144).
-define(CLIENT_PLUGIN_AUTH, 524288).
-define(CLIENT_CONNECT_ATTRS, 1048576).

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
                                        % based on MySQL version 5.5.30 using ?COM_QUERY
-define(FIELD_TYPE_DECIMAL, 16#00).     % unused ???
-define(FIELD_TYPE_TINY, 16#01).        % TINYINT
-define(FIELD_TYPE_SHORT, 16#02).       % SMALLINT
-define(FIELD_TYPE_LONG, 16#03).        % INT
-define(FIELD_TYPE_FLOAT, 16#04).       % FLOAT(M) M =< 24
-define(FIELD_TYPE_DOUBLE, 16#05).      % DOUBLE, FLOAT(M) M > 24
-define(FIELD_TYPE_NULL, 16#06).        % generally unused? query "SELECT NULL" will return this.
-define(FIELD_TYPE_TIMESTAMP, 16#07).   % TIMESTAMP
-define(FIELD_TYPE_LONGLONG, 16#08).    % BIGINT
-define(FIELD_TYPE_INT24, 16#09).       % MEDIUMINT
-define(FIELD_TYPE_DATE, 16#0a).        % DATE
-define(FIELD_TYPE_TIME, 16#0b).        % TIME
-define(FIELD_TYPE_DATETIME, 16#0c).    % DATETIME
-define(FIELD_TYPE_YEAR, 16#0d).        % YEAR
-define(FIELD_TYPE_NEWDATE, 16#0e).     % unused ???
-define(FIELD_TYPE_VARCHAR, 16#0f).     % unused ???
-define(FIELD_TYPE_BIT, 16#10).         % BIT
-define(FIELD_TYPE_NEWDECIMAL, 16#f6).  % DECIMAL
-define(FIELD_TYPE_ENUM, 16#f7).        % unused ???
-define(FIELD_TYPE_SET, 16#f8).         % unused ???
-define(FIELD_TYPE_TINY_BLOB, 16#f9).   % unused ???
-define(FIELD_TYPE_MEDIUM_BLOB, 16#fa). % unused ???
-define(FIELD_TYPE_LONG_BLOB, 16#fb).   % unused ???
-define(FIELD_TYPE_BLOB, 16#fc).        % TINYBLOB, MEDIUMBLOB, BLOB, LONGBLOB, TINYTEXT, MEDIUMTEXT, TEXT, LONGTEXT
-define(FIELD_TYPE_VAR_STRING, 16#fd).  % VARCHAR, VARBINARY
-define(FIELD_TYPE_STRING, 16#fe).      % CHAR, BINARY, ENUM, SET
-define(FIELD_TYPE_GEOMETRY, 16#ff).    % unused ???

%% MSQL SERVER STATES (mysql_com.h)
-define(SERVER_NO_STATUS, 0).
-define(SERVER_STATUS_IN_TRANS, 1).	% Transaction has started
-define(SERVER_STATUS_AUTOCOMMIT, 2). % Server in auto_commit mode
-define(SERVER_MORE_RESULTS_EXIST, 8). % Multi query - next query exists
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
-define(SERVER_QUERY_WAS_SLOW, 2048).
-define(SERVER_PS_OUT_PARAMS, 4096).

%% RESPONSE
-define(RESP_OK, 0).
-define(RESP_INFILE, 251).
-define(RESP_EOF, 254).
-define(RESP_ERROR, 255).

%% AUTH PLUGIN
-define(MYSQL_NATIVE_PASSWORD, <<"mysql_native_password">>).
-define(MYSQL_OLD_PASSWORD, <<"mysql_old_password">>).
