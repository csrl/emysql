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
-module(emysql_conn).

-export([
  close/1, open/7, execute/2
]).

-include("emysql.hrl").

-define(GREETING_TIMEOUT, 5000). %% milliseconds
-define(MAXPACKETBYTES, 16#00FFFFFF). %% 16MB per protocol spec
-define(CLIENT_CAPS_REQUIRED, (
  ?CLIENT_LONG_PASSWORD bor ?CLIENT_LONG_FLAG bor ?CLIENT_TRANSACTIONS bor
  ?CLIENT_PROTOCOL_41 bor ?CLIENT_CONNECT_WITH_DB
)).
-define(CLIENT_CAPS_OPTIONAL, (
  ?CLIENT_MULTI_STATEMENTS bor ?CLIENT_MULTI_RESULTS bor
  ?CLIENT_SECURE_CONNECTION)).
-define(CLIENT_CAPS, (?CLIENT_CAPS_REQUIRED bor ?CLIENT_CAPS_OPTIONAL)).

%%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

%%------------------------------------------------------------------------------
close(#connection{socket = Sock}) ->
  emysql_tcp:close(Sock),
  ok.

%%------------------------------------------------------------------------------
open(PoolId, User, Password, Host, Port, Database, Collation) ->
  Sock = emysql_tcp:connect(Host, Port),
  try
    ThreadID = greet(Sock, User, Password, Database, Collation),
    #connection{socket = Sock, pool_id = PoolId, thread_id = ThreadID}
  catch
    {Who, _Error} = What when open =:= Who orelse tcp =:= Who ->
      emysql_tcp:close(Sock),
      throw(What)
  end.

%%------------------------------------------------------------------------------
execute(#connection{socket = Sock}, Query) ->
  emysql_tcp:send_and_recv(Sock, [?COM_QUERY, Query], 0).

%%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

%%------------------------------------------------------------------------------
greet(Sock, User, Password, Database, Collation) ->
  case emysql_tcp:recv_greeting(Sock, ?GREETING_TIMEOUT) of
    #greeting{} = Greeting ->
      case handshake(Sock, User, Password, Database, Collation, Greeting) of
        #ok_packet{} ->
          Greeting#greeting.thread_id;
        #error_packet{msg = Msg} ->
          throw({open, {failed_to_authenticate, Msg}})
      end;
    #error_packet{msg = Msg} ->
      throw({open, {failed_to_greet, Msg}})
  end.

%%------------------------------------------------------------------------------
handshake(Sock, User, Password, Database, undefined, Greeting) ->
  handshake(
    Sock, User, Password, Database, Greeting#greeting.collation, Greeting);

handshake(Sock, User, Password, Database, Collation, Greeting) ->
  #greeting{
    seq_num=SeqNum, caps=ServerCaps, salt=Salt, plugin=Plugin} = Greeting,
  Caps = validate_server_caps(ServerCaps),
  Maxsize = ?MAXPACKETBYTES,
  ScrambleBuff = password(Plugin, Password, Salt),
  ScrambleLen = size(ScrambleBuff),
  Packet = [
    <<Caps:32/little, Maxsize:32/little, Collation:8, 0:23/unit:8>>,
    User, 0, ScrambleLen, ScrambleBuff, Database, 0],
  case emysql_tcp:send_and_recv(Sock, Packet, SeqNum+1) of
    #eof_packet{seq_num = SeqNum1} ->
      AuthOld = password(?MYSQL_OLD_PASSWORD, Password, Salt),
      emysql_tcp:send_and_recv(Sock, [AuthOld, 0], SeqNum1+1);
    Result ->
      Result
  end.

%%------------------------------------------------------------------------------
password(?MYSQL_OLD_PASSWORD, Password, Salt) ->
  {P1, P2} = hash(Password),
  {S1, S2} = hash(Salt),
  Seed1 = P1 bxor S1,
  Seed2 = P2 bxor S2,
  List = rnd(9, Seed1, Seed2),
  {L, [Extra]} = lists:split(8, List),
  list_to_binary(lists:map(fun (E) -> E bxor (Extra - 64) end, L));

password(?MYSQL_NATIVE_PASSWORD, Password, Salt) ->
  Stage1 = crypto:sha(Password),
  Stage2 = crypto:sha(Stage1),
  Res = crypto:sha_final(
    crypto:sha_update(crypto:sha_update(crypto:sha_init(),Salt),Stage2)),
  bxor_binary(Res, Stage1).

%%------------------------------------------------------------------------------
validate_server_caps(ServerCaps) ->
  case ServerCaps band ?CLIENT_CAPS_REQUIRED of
    ?CLIENT_CAPS_REQUIRED ->
      ServerCaps band ?CLIENT_CAPS;
    _ ->
      throw({open, {server_capabilities_mismatch, ServerCaps}})
  end.

%%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%% Native Password helper functions
%%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

%%------------------------------------------------------------------------------
bxor_binary(B1, B2) ->
  list_to_binary(dualmap(
    fun (E1, E2) -> E1 bxor E2 end, binary_to_list(B1), binary_to_list(B2))).

dualmap(_F, [], []) ->
  [];
dualmap(F, [E1 | R1], [E2 | R2]) ->
  [F(E1, E2) | dualmap(F, R1, R2)].

%%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%% Old Password helper functions
%%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

%%------------------------------------------------------------------------------
hash(S) -> hash(binary_to_list(iolist_to_binary(S)), 1345345333, 305419889, 7).
hash([C | S], N1, N2, Add) ->
  N1_1 = N1 bxor (((N1 band 63) + Add) * C + N1 * 256),
  N2_1 = N2 + ((N2 * 256) bxor N1_1),
  Add_1 = Add + C,
  hash(S, N1_1, N2_1, Add_1);
hash([], N1, N2, _Add) ->
  Mask = (1 bsl 31) - 1,
  {N1 band Mask, N2 band Mask}.

%%------------------------------------------------------------------------------
rnd(N, Seed1, Seed2) ->
  Mod = (1 bsl 30) - 1,
  rnd(N, [], Seed1 rem Mod, Seed2 rem Mod).
rnd(0, List, _, _) ->
  lists:reverse(List);
rnd(N, List, Seed1, Seed2) ->
  Mod = (1 bsl 30) - 1,
  NSeed1 = (Seed1 * 3 + Seed2) rem Mod,
  NSeed2 = (NSeed1 + Seed2 + 33) rem Mod,
  Float = (float(NSeed1) / float(Mod))*31,
  Val = trunc(Float)+64,
  rnd(N - 1, [Val | List], NSeed1, NSeed2).
