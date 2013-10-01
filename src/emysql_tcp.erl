%% Copyright (c) 2009-2011
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
-module(emysql_tcp).

-export([close/1, connect/2, recv_greeting/2, send_and_recv/3]).

-include("emysql.hrl").

%% Default collation will mostly go unused on newer mysql servers, as with them
%% we just default to what the server wants.
-define(DEFAULT_COLLATION, 8). %% 8 = latin1_swedish_ci, 192 = utf8_unicode_ci

%%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

%%------------------------------------------------------------------------------
close(Sock) ->
  gen_tcp:close(Sock).

%%------------------------------------------------------------------------------
connect(Host, Port) ->
  case gen_tcp:connect(Host, Port, [binary, {packet, raw}, {active, false}]) of
    {ok, Sock} ->
      Sock;
    {error, Reason} ->
      throw({tcp, {failed_to_connect, Reason}})
  end.

%%------------------------------------------------------------------------------
recv_greeting(Sock, Timeout) ->
  {Data, SeqNum, <<>>} = recv_packet(Sock, Timeout, <<>>),
  make_greeting(Data, SeqNum).

%%------------------------------------------------------------------------------
send_and_recv(Sock, Data, SeqNum) ->
  Packet = [<<(iolist_size(Data)):24/little, SeqNum:8>>, Data],
  case gen_tcp:send(Sock, Packet) of
    ok ->
      process_response(Sock);
    {error, Reason} ->
      throw({tcp, {failed_send, Reason}})
  end.

%%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

%%------------------------------------------------------------------------------
recv_packet(_Sock, _Timeout, <<Len:24/little, SeqNum:8, Data:Len/binary, Bin/binary>>) ->
  {Data, SeqNum, Bin};

recv_packet(Sock, Timeout, <<Len:24/little, _SeqNum:8, Rest/binary>> = Bin) ->
  %% The idea here is, inet tcp will do the accumulation for large packets
  recv_packet(Sock, Timeout, <<Bin/binary, (recv(Sock, Len - size(Rest), infinity))/binary>>);

recv_packet(Sock, Timeout, Bin) ->
  recv_packet(Sock, Timeout, <<Bin/binary, (recv(Sock, 0, Timeout))/binary>>).

%%------------------------------------------------------------------------------
recv(Sock, Requested, Timeout) ->
  case gen_tcp:recv(Sock, Requested, Timeout) of
    {ok, Bin} ->
      Bin;
    {error, Reason} ->
      throw({tcp, {failed_recv, Reason}})
  end.

%%------------------------------------------------------------------------------
make_greeting(Data, SeqNum) ->
  {ProtocolVersion, ServerVersion, ThreadID, Salt1,
    ServerCapsL, More} = greeting_base(Data),
  {ServerCollation, ServerStatus, ServerCapsH,
    ScrambleLength, Secure} = greeting_more(More),
  ServerCaps = ServerCapsL bor (ServerCapsH bsl 16),
  {Salt2, Plugin} = greeting_secure(ServerCaps, ScrambleLength, Secure),
  #greeting{
    seq_num = SeqNum,
    protocol_version = ProtocolVersion,
    server_version = ServerVersion,
    thread_id = ThreadID,
    salt = [Salt1, Salt2],
    caps = ServerCaps,
    collation = ServerCollation,
    status = ServerStatus,
    plugin = Plugin}.

greeting_base(<<ProtocolVersion:8, Rest1/binary>>) ->
  {ServerVersion, Rest2} = asciz(Rest1),
  <<ThreadID:32/little, Salt1:8/binary, 0, ServerCapsL:16/little,
    More/binary>> = Rest2,
  {ProtocolVersion, ServerVersion, ThreadID, Salt1, ServerCapsL, More}.

greeting_more(<<>>) ->
  {?DEFAULT_COLLATION, 0, 0, 0, <<>>};
greeting_more(More) ->
  <<ServerCollation:8/little, ServerStatus:16/little, ServerCapsH:16/little,
    ScrambleLength:8/little, 0:10/unit:8, Secure/binary>> = More,
  {ServerCollation, ServerStatus, ServerCapsH, ScrambleLength, Secure}.

greeting_secure(_ServerCaps, 0, <<>>) ->
  {<<>>, <<>>};
greeting_secure(ServerCaps, ScrambleLength, Secure) ->
  case ?CLIENT_SECURE_CONNECTION band ServerCaps of
    ?CLIENT_SECURE_CONNECTION ->
      Salt2Length = max(13, ScrambleLength - 8),
      <<Salt20:Salt2Length/binary, Plugin0/binary>> = Secure,
      {Salt2, <<>>} = asciz(Salt20), %% trim trailing 0 if exists
      {Plugin, <<>>} = asciz(Plugin0), %% trim trailing 0 if exists
      {Salt2, Plugin};
    _ ->
      {<<>>, <<>>}
  end.

%%------------------------------------------------------------------------------
process_response(Sock) ->
  case response_list(Sock, <<>>, ?SERVER_MORE_RESULTS_EXIST) of
    [Record | []] -> Record;
    List -> List
  end.

%%------------------------------------------------------------------------------
response_list(_Sock, _Bin, 0) -> [];

response_list(Sock, Bin, ?SERVER_MORE_RESULTS_EXIST) ->
  {Data, SeqNum, MoreBin1} = recv_packet(Sock, infinity, Bin),
  {Response, ServerStatus, MoreBin2} = handle_response(
    Sock, Data, SeqNum, MoreBin1),
  [Response | response_list(
    Sock, MoreBin2, ServerStatus band ?SERVER_MORE_RESULTS_EXIST)].

%%------------------------------------------------------------------------------
handle_response(_Sock, <<?RESP_OK, Rest/binary>>, SeqNum, Bin) ->
  {AffectedRows, Rest1} = length_coded_integer(Rest),
  {InsertId, Rest2} = length_coded_integer(Rest1),
  <<ServerStatus:16/little, WarningCount:16/little, Msg/binary>> = Rest2,
  {#ok_packet{
    seq_num = SeqNum,
    affected_rows = AffectedRows,
    insert_id = InsertId,
    status = ServerStatus,
    warning_count = WarningCount,
    msg = Msg},
  ServerStatus, Bin};

%% This is only needed here for the old password fallback handling during
%% connection handshake.
handle_response(_Sock, <<?RESP_EOF, WarningCount:16/little, ServerStatus:16/little>>, SeqNum, Bin) ->
  {#eof_packet{seq_num = SeqNum, status = ServerStatus, warning_count = WarningCount},
    ServerStatus, Bin};

handle_response(_Sock, <<?RESP_ERROR, ErrNo:16/little, "#", SQLState:5/binary, Msg/binary>>, SeqNum, Bin) ->
  {#error_packet{seq_num = SeqNum, code = ErrNo, status = SQLState, msg = Msg},
    ?SERVER_NO_STATUS, Bin};

handle_response(_Sock, <<?RESP_INFILE, Filename/binary>>, _SeqNum, _Bin) ->
  throw({tcp, {unsupported_local_infile, Filename}});

%% This only works for COM_QUERY.
handle_response(Sock, Data, _SeqNum, Bin) ->
  {FieldCount, <<>>} = length_coded_integer(Data),
  {Fields, _SeqNum1, _ServerStatus1, MoreBin1} = recv_fields(
    Sock, FieldCount, [], Bin),
  case recv_rows(
      Sock, [Field#field.type || Field <- Fields], [], MoreBin1) of
    {Rows, SeqNum2, ServerStatus2, MoreBin2} ->
      {#result_packet{seq_num = SeqNum2, fields = Fields, rows = Rows},
        ServerStatus2, MoreBin2};
    ErrorResult ->
      ErrorResult
  end.

%%------------------------------------------------------------------------------
recv_fields(Sock, 0, Fields, Bin) ->
  {<<?RESP_EOF, _WarningCount:16/little, ServerStatus:16/little>>,
    SeqNum, MoreBin} = recv_packet(Sock, infinity, Bin),
  {lists:reverse(Fields ,[]), SeqNum, ServerStatus, MoreBin};

recv_fields(Sock, FieldCount, Fields, Bin) ->
  {Data, SeqNum, MoreBin} = recv_packet(Sock, infinity, Bin),
  recv_fields(Sock, FieldCount-1, [make_field(SeqNum, Data) | Fields], MoreBin).

%%------------------------------------------------------------------------------
recv_rows(Sock, Types, Acc, Bin) ->
  {Data, SeqNum, MoreBin} = recv_packet(Sock, infinity, Bin),
  case Data of
    <<?RESP_EOF, _WarningCount:16/little, ServerStatus:16/little>> ->
      {lists:reverse(Acc, []), SeqNum, ServerStatus, MoreBin};
    <<?RESP_ERROR, ErrNo:16/little, "#", SQLState:5/binary, Msg/binary>> ->
      {#error_packet{
        seq_num = SeqNum,
        code = ErrNo,
        status = SQLState,
        msg = Msg}, ?SERVER_NO_STATUS, MoreBin};
    _ ->
      recv_rows(Sock, Types, [decode_row_data(Data, Types) | Acc], MoreBin)
  end.

%%------------------------------------------------------------------------------
decode_row_data(<<>>, []) ->
  [];

decode_row_data(Data, [Type | Types]) ->
  {String, Rest} = length_coded_string(Data),
  [type_cast_data(Type, String) | decode_row_data(Rest, Types)].

%%------------------------------------------------------------------------------
make_field(SeqNum, Data) ->
  %% A little uglier, but 100% speed improvement, and as far as I know these
  %% can't be null.  Table and Name still have to be decoded, since they can
  %% be more than 250 bytes.
  <<3, "def", L1, Db:L1/binary, Rest1/binary>> = Data,
  {Table, <<L2, OrgTable:L2/binary, Rest2/binary>>} = length_coded_string(Rest1),
  {Name, <<L3, OrgName:L3/binary, Rest3/binary>>} = length_coded_string(Rest2),
%  {_Catalog, Rest1} = length_coded_string(Data), % 'def'
%  {Db, Rest2} = length_coded_string(Rest1), % max 64 chars
%  {Table, Rest3} = length_coded_string(Rest2), % max 256 chars
%  {OrgTable, Rest4} = length_coded_string(Rest3), % max 64 chars
%  {Name, Rest5} = length_coded_string(Rest4), % max 256 chars
%  {OrgName, Rest6} = length_coded_string(Rest5), % max 64 chars
  <<12, % num bytes of fixed size fields; defined as a length coded integer
    CharSetNr:16/little,
    Length:32/little,
    Type:8/little,
    Flags:16/little,
    Decimals:8/little,
    0:16, Rest4/binary>> = Rest3,
  {Defaults, <<>>} = case Rest4 of
    <<>> -> {<<>>, <<>>};
    _ -> length_coded_string(Rest4)
  end,
  #field{
    seq_num = SeqNum,
    db = Db,
    table = Table,
    org_table = OrgTable,
    name = Name,
    org_name = OrgName,
    type = Type,
    default = Defaults,
    charset_nr = CharSetNr,
    length = Length,
    flags = Flags,
    decimals = Decimals}.

%%------------------------------------------------------------------------------
%% TODO: use Field length, flags, decimals to properly format various types
%%       eg. float precision, zerofill etc.
type_cast_data(_Type ,undefined) ->
  % ?FIELD_TYPE_NULL; type can be anything here, including FIELD_TYPE_NULL.
  undefined;

type_cast_data(Type, String) when
    Type == ?FIELD_TYPE_TINY;
    Type == ?FIELD_TYPE_SHORT;
    Type == ?FIELD_TYPE_LONG;
    Type == ?FIELD_TYPE_LONGLONG;
    Type == ?FIELD_TYPE_INT24;
    Type == ?FIELD_TYPE_YEAR ->
  binary_to_integer(String);

type_cast_data(Type, String) when
    Type == ?FIELD_TYPE_DECIMAL; % unused???
    Type == ?FIELD_TYPE_NEWDECIMAL;
    Type == ?FIELD_TYPE_FLOAT;
    Type == ?FIELD_TYPE_DOUBLE ->
  type_cast_number(String);

type_cast_data(Type, String) when
    Type == ?FIELD_TYPE_DATE ->
  type_cast_date(String);

type_cast_data(Type, String) when
    Type == ?FIELD_TYPE_TIME ->
  type_cast_time(String);

type_cast_data(Type, String) when
    Type == ?FIELD_TYPE_TIMESTAMP;
    Type == ?FIELD_TYPE_DATETIME ->
  type_cast_datetime(String);

type_cast_data(Type, String) when
    Type == ?FIELD_TYPE_BIT ->
  type_cast_bit(String);

% TODO:
% ?FIELD_TYPE_NEWDATE % unused???
% ?FIELD_TYPE_ENUM % unused???
% ?FIELD_TYPE_SET % unused???
% ?FIELD_TYPE_GEOMETRY % unused???
type_cast_data(_Type, String) ->
  %  Type == ?FIELD_TYPE_VARCHAR; % unused???
  %  Type == ?FIELD_TYPE_TINY_BLOB; % unused???
  %  Type == ?FIELD_TYPE_MEDIUM_BLOB; % unused???
  %  Type == ?FIELD_TYPE_LONG_BLOB; % unused???
  %  Type == ?FIELD_TYPE_BLOB;
  %  Type == ?FIELD_TYPE_VAR_STRING;
  %  Type == ?FIELD_TYPE_STRING ->
 String.

%%------------------------------------------------------------------------------
%type_cast_nop(Data) ->
%  Data.

%%------------------------------------------------------------------------------
type_cast_number(Data) ->
  LData = binary_to_list(Data),
  {ok, [Num], _Leftovers} = case io_lib:fread("~f", LData) of
    {error, _} ->
      case io_lib:fread("~d", LData) of
        {ok, [_], []} = Res ->
          Res;
        {ok, [X], E} ->
          io_lib:fread("~f", lists:flatten(io_lib:format("~w~s~s", [X,".0",E])))
        end
    ;
    Res ->
      Res
  end,
  Num.

%%------------------------------------------------------------------------------
type_cast_date(<<Y:4/binary, "-", M:2/binary, "-", D:2/binary>>) ->
  {binary_to_integer(Y), binary_to_integer(M), binary_to_integer(D)}.

%%------------------------------------------------------------------------------
type_cast_time(<<H:2/binary, ":", M:2/binary, ":", S:2/binary>>) ->
  {binary_to_integer(H), binary_to_integer(M), binary_to_integer(S)}.

%%------------------------------------------------------------------------------
type_cast_datetime(<<D:10/binary, " ", T:8/binary>>) ->
  {type_cast_date(D), type_cast_time(T)}.

%%------------------------------------------------------------------------------
type_cast_bit(Data) ->
  Bits = bit_size(Data),
  <<Num:Bits/little>> = Data,
  Num.

%%------------------------------------------------------------------------------
length_coded_integer(<<252, Int:16/little, Tail/binary>>) ->
  {Int, Tail};
length_coded_integer(<<253, Int:24/little, Tail/binary>>) ->
  {Int, Tail};
length_coded_integer(<<254, Int:64/little, Tail/binary>>) ->
  {Int, Tail};
length_coded_integer(<<Int:8, Tail/binary>>) when Int =< 250 ->
  {Int, Tail}.

%%------------------------------------------------------------------------------
length_coded_string(<<251, Tail/binary>>) ->
  {undefined, Tail};
length_coded_string(<<252, Len:16/little, Str:Len/binary, Tail/binary>>) ->
  {Str, Tail};
length_coded_string(<<253, Len:24/little, Str:Len/binary, Tail/binary>>) ->
  {Str, Tail};
length_coded_string(<<254, Len:64/little, Str:Len/binary, Tail/binary>>) ->
  {Str, Tail};
length_coded_string(<<Len:8, Str:Len/binary, Tail/binary>>) when Len =< 250 ->
  {Str, Tail}.

%%------------------------------------------------------------------------------
asciz(Data) ->
  case binary:split(Data, <<0>>) of
    [L, R] -> {L, R};
    [L] -> {L, <<>>}
  end.

% ------------------------------------------------------------------------------
% http://dev.mysql.com/doc/internals/en/generic-response-packets.html
%
% ------------------------------------------------------------------------------
% OK packet format
% ------------------------------------------------------------------------------
%
%  VERSION 4.1
%  Bytes                       Name
%  -----                       ----
%  1   (Length Coded Integer)  field_count, always = 0
%  1-9 (Length Coded Integer)  affected_rows
%  1-9 (Length Coded Integer)  insert_id
%  2                           server_status
%  2                           warning_count
%  n   (until end of packet)   message
%
%  field_count:     always = 0
%
%  affected_rows:   = number of rows affected by INSERT/UPDATE/DELETE
%
%  insert_id:       If the statement generated any AUTO_INCREMENT number,
%                   it is returned here. Otherwise this field contains 0.
%                   Note: when using for example a multiple row INSERT the
%                   insert_id will be from the first row inserted, not from
%                   last.
%
%  server_status:   = The client can use this to check if the
%                   command was inside a transaction.
%
%  warning_count:   number of warnings
%
%  message:         For example, after a multi-line INSERT, message might be
%                   "Records: 3 Duplicates: 0 Warnings: 0"
%
% ------------------------------------------------------------------------------
% Error packet format
% ------------------------------------------------------------------------------
%
%  VERSION 4.1
%  Bytes                       Name
%  -----                       ----
%  1                           field_count, always = 0xff
%  2                           errno
%  1                           (sqlstate marker), always '#'
%  5                           sqlstate (5 characters)
%  n   (until end of packet)   message
%
%  field_count:     Always 0xff (255 decimal).
%
%  errno:           The possible values are listed in the manual, and in the
%                   MySQL source code file /include/mysqld_error.h.
%
%  sqlstate marker: This is always '#'. It is necessary for distinguishing
%                   version-4.1 messages.
%
%  sqlstate:        The server translates errno values to sqlstate values with a
%                   function named mysql_errno_to_sqlstate(). The possible
%                   values are listed in the manual, and in the MySQL source
%                   code file /include/sql_state.h.
%
%  message:         The error message is a string which ends at the end of the
%                   packet, that is, its length can be determined from the
%                   packet header.
%                   Expect the message to be between 0 and 512 bytes long.
%
% ------------------------------------------------------------------------------
% EOF packet format
% ------------------------------------------------------------------------------
%
%  VERSION 4.1
%  Bytes                       Name
%  -----                       ----
%  1                           field_count, always = 0xfe
%  2                           warning_count
%  2                           Status Flags
%
%  field_count:     The value is always 0xfe (decimal 254). However, the value
%                   254 can begin a Length-Encoded-Integer value which contains
%                   an 8-byte integer. So, to ensure that a packet is really an
%                   EOF Packet:
%                     (a) check that first byte in packet = 0xfe
%                     (b) check that size of packet smaller than 9
%
%  warning_count:   Number of warnings. Sent after all data has been sent to the
%                   client.
%
%  server_status:   Contains flags like SERVER_MORE_RESULTS_EXISTS
