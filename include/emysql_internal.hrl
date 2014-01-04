%% Copyright (c) 2009-2012
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

-include("emysql.hrl").

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
  seq_num, plugin, salt}).

%% MYSQL CLIENT/SERVER CAPABILITIES
-define(CLIENT_LONG_PASSWORD,           16#1).
-define(CLIENT_FOUND_ROWS,              16#2).          % Not Implemented
-define(CLIENT_LONG_FLAG,               16#4).
-define(CLIENT_CONNECT_WITH_DB,         16#8).
-define(CLIENT_NO_SCHEMA,               16#10).         % Not Implemented
-define(CLIENT_COMPRESS,                16#20).         % Not Implemented
-define(CLIENT_ODBC,                    16#40).         % Obsolete
-define(CLIENT_LOCAL_FILES,             16#80).         % Not Implemented
-define(CLIENT_IGNORE_SPACE,            16#100).        % Not Implemented
-define(CLIENT_PROTOCOL_41,             16#200).
-define(CLIENT_INTERACTIVE,             16#400).        % Not Implemented
-define(CLIENT_SSL,                     16#800).        % Not Implemented
-define(CLIENT_IGNORE_SIGPIPE,          16#1000).       % Not Applicable
-define(CLIENT_TRANSACTIONS,            16#2000).
-define(CLIENT_RESERVED,                16#4000).       % Obsolete
-define(CLIENT_SECURE_CONNECTION,       16#8000).
-define(CLIENT_MULTI_STATEMENTS,        16#10000).
-define(CLIENT_MULTI_RESULTS,           16#20000).
-define(CLIENT_PS_MULTI_RESULTS,        16#40000).
-define(CLIENT_PLUGIN_AUTH,             16#80000).
-define(CLIENT_CONNECT_ATTRS,           16#100000).

%% MYSQL COMMANDS
-define(COM_SLEEP,                      16#00).
-define(COM_QUIT,                       16#01).
-define(COM_INIT_DB,                    16#02).
-define(COM_QUERY,                      16#03).     % Implemented
-define(COM_FIELD_LIST,                 16#04).
-define(COM_CREATE_DB,                  16#05).
-define(COM_DROP_DB,                    16#06).
-define(COM_REFRESH,                    16#07).
-define(COM_SHUTDOWN,                   16#08).
-define(COM_STATISTICS,                 16#09).
-define(COM_PROCESS_INFO,               16#0a).
-define(COM_CONNECT,                    16#0b).
-define(COM_PROCESS_KILL,               16#0c).
-define(COM_DEBUG,                      16#0d).
-define(COM_PING,                       16#0e).
-define(COM_TIME,                       16#0f).
-define(COM_DELAYED_INSERT,             16#10).
-define(COM_CHANGE_USER,                16#11).
-define(COM_BINLOG_DUMP,                16#12).
-define(COM_TABLE_DUMP,                 16#13).
-define(COM_CONNECT_OUT,                16#14).
-define(COM_REGISTER_SLAVE,             16#15).
-define(COM_STMT_PREPARE,               16#16).
-define(COM_STMT_EXECUTE,               16#17).
-define(COM_STMT_SEND_LONG_DATA,        16#18).
-define(COM_STMT_CLOSE,                 16#19).
-define(COM_STMT_RESET,                 16#1a).
-define(COM_SET_OPTION,                 16#1b).
-define(COM_STMT_FETCH,                 16#1c).

%% MSQL SERVER STATES
-define(SERVER_NO_STATUS,                   16#0).
-define(SERVER_STATUS_IN_TRANS,             16#1).
-define(SERVER_STATUS_AUTOCOMMIT,           16#2).
-define(SERVER_MORE_RESULTS_EXIST,          16#8).      % Multi query - next query result exists
-define(SERVER_QUERY_NO_GOOD_INDEX_USED,    16#10).
-define(SERVER_QUERY_NO_INDEX_USED,         16#20).
-define(SERVER_STATUS_CURSOR_EXISTS,        16#40).
-define(SERVER_STATUS_LAST_ROW_SENT,        16#80).
-define(SERVER_STATUS_DB_DROPPED,           16#100).
-define(SERVER_STATUS_NO_BACKSLASH_ESCAPES, 16#200).
-define(SERVER_STATUS_METADATA_CHANGED,     16#400).
-define(SERVER_QUERY_WAS_SLOW,              16#800).
-define(SERVER_PS_OUT_PARAMS,               16#1000).

%% RESPONSE
-define(RESP_OK,      0).
-define(RESP_INFILE,  251).
-define(RESP_EOF,     254).
-define(RESP_ERROR,   255).
