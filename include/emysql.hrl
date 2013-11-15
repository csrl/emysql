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

-record(ok_packet, {
  seq_num, affected_rows, insert_id, status, warning_count, msg}).
-record(error_packet, {
  seq_num, code, status, msg}).
-record(result_packet, {
  seq_num, fields, rows}).
-record(field, {
  seq_num, db, table, org_table, name, org_name, type, default,
  charset_nr, length, flags, decimals}).

%% MYSQL TYPES                                      % based on MySQL version 5.5.30 using ?COM_QUERY
-define(FIELD_TYPE_DECIMAL,             16#00).     % unused ???
-define(FIELD_TYPE_TINY,                16#01).     % TINYINT
-define(FIELD_TYPE_SHORT,               16#02).     % SMALLINT
-define(FIELD_TYPE_LONG,                16#03).     % INT
-define(FIELD_TYPE_FLOAT,               16#04).     % FLOAT(M) M =< 24
-define(FIELD_TYPE_DOUBLE,              16#05).     % DOUBLE, FLOAT(M) M > 24
-define(FIELD_TYPE_NULL,                16#06).     % generally unused? query "SELECT NULL" will return this.
-define(FIELD_TYPE_TIMESTAMP,           16#07).     % TIMESTAMP
-define(FIELD_TYPE_LONGLONG,            16#08).     % BIGINT
-define(FIELD_TYPE_INT24,               16#09).     % MEDIUMINT
-define(FIELD_TYPE_DATE,                16#0a).     % DATE
-define(FIELD_TYPE_TIME,                16#0b).     % TIME
-define(FIELD_TYPE_DATETIME,            16#0c).     % DATETIME
-define(FIELD_TYPE_YEAR,                16#0d).     % YEAR
-define(FIELD_TYPE_NEWDATE,             16#0e).     % unused ???
-define(FIELD_TYPE_VARCHAR,             16#0f).     % unused ???
-define(FIELD_TYPE_BIT,                 16#10).     % BIT
-define(FIELD_TYPE_NEWDECIMAL,          16#f6).     % DECIMAL
-define(FIELD_TYPE_ENUM,                16#f7).     % unused ???
-define(FIELD_TYPE_SET,                 16#f8).     % unused ???
-define(FIELD_TYPE_TINY_BLOB,           16#f9).     % unused ???
-define(FIELD_TYPE_MEDIUM_BLOB,         16#fa).     % unused ???
-define(FIELD_TYPE_LONG_BLOB,           16#fb).     % unused ???
-define(FIELD_TYPE_BLOB,                16#fc).     % TINYBLOB, MEDIUMBLOB, BLOB, LONGBLOB, TINYTEXT, MEDIUMTEXT, TEXT, LONGTEXT
-define(FIELD_TYPE_VAR_STRING,          16#fd).     % VARCHAR, VARBINARY
-define(FIELD_TYPE_STRING,              16#fe).     % CHAR, BINARY, ENUM, SET
-define(FIELD_TYPE_GEOMETRY,            16#ff).     % unused ???
