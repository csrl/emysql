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
  close/1, open/6, query/2
]).

-include("emysql_internal.hrl").

-define(GREETING_TIMEOUT, 5000). %% milliseconds
-define(MAXPACKETBYTES, 16#00FFFFFF). %% 16MB per protocol spec
-define(CLIENT_CAPS_REQUIRED, (
  ?CLIENT_LONG_PASSWORD bor ?CLIENT_LONG_FLAG bor ?CLIENT_TRANSACTIONS bor
  ?CLIENT_PROTOCOL_41 bor ?CLIENT_SECURE_CONNECTION bor ?CLIENT_CONNECT_WITH_DB
  )).
-define(CLIENT_CAPS_OPTIONAL, (
  ?CLIENT_MULTI_STATEMENTS bor ?CLIENT_MULTI_RESULTS bor ?CLIENT_PLUGIN_AUTH)).
-define(CLIENT_CAPS, (?CLIENT_CAPS_REQUIRED bor ?CLIENT_CAPS_OPTIONAL)).

%%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

%%------------------------------------------------------------------------------
close(#connection{socket = Sock}) ->
  emysql_tcp:close(Sock),
  ok.

%%------------------------------------------------------------------------------
open(User, Password, Host, Port, Database, Collation) ->
  Sock = emysql_tcp:connect(Host, Port),
  try
    ThreadID = greet(Sock, User, Password, Database, Collation),
    #connection{socket = Sock, thread_id = ThreadID}
  catch
    {Who, _Error} = What when open =:= Who orelse tcp =:= Who ->
      emysql_tcp:close(Sock),
      throw(What)
  end.

%%------------------------------------------------------------------------------
query(#connection{socket = Sock}, Query) ->
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
    seq_num=SeqNum, caps=ServerCaps, salt=Salt, plugin=Plugin0} = Greeting,
  Caps = validate_server_caps(ServerCaps),
  Maxsize = ?MAXPACKETBYTES,
  Plugin = case Plugin0 of
    <<>> -> ?MYSQL_NATIVE_PASSWORD;
    _ -> Plugin0
  end,
  ScrambleBuff = password(Plugin, Password, Salt),
  ScrambleLen = size(ScrambleBuff),
  PluginClient = case ?CLIENT_PLUGIN_AUTH band Caps of
    ?CLIENT_PLUGIN_AUTH -> [Plugin, 0];
    0 -> <<>>
  end,
  Packet = [
    <<Caps:32/little, Maxsize:32/little, Collation:8, 0:23/unit:8>>,
    User, 0, ScrambleLen, ScrambleBuff, Database, 0, PluginClient],
  case emysql_tcp:send_and_recv(Sock, Packet, SeqNum+1) of
    #authswitch{seq_num = SeqNum1, plugin = <<>>, salt = <<>>} ->
      Rescramble = password(?MYSQL_OLD_PASSWORD, Password, Salt),
      emysql_tcp:send_and_recv(Sock, [Rescramble, 0], SeqNum1+1);
    #authswitch{seq_num = SeqNum1, plugin = Plugin1, salt = Salt1} ->
      Rescramble = password(Plugin1, Password, Salt1),
      emysql_tcp:send_and_recv(Sock, [Rescramble, 0], SeqNum1+1);
    Result ->
      Result
  end.

%%------------------------------------------------------------------------------
password(?MYSQL_OLD_PASSWORD, Password, Salt) ->
  Salt1 = binary:part(Salt, 0, 8), % only use first 8 bytes of salt
  {P1, P2} = hash(Password),
  {S1, S2} = hash(Salt1),
  Seed1 = P1 bxor S1,
  Seed2 = P2 bxor S2,
  List = rnd(9, Seed1, Seed2),
  {L, [Extra]} = lists:split(8, List),
  list_to_binary(lists:map(fun (E) -> (E + 64) bxor Extra end, L));

password(?MYSQL_NATIVE_PASSWORD, Password, Salt) ->
  Stage1 = crypto:hash(sha, Password),
  Stage2 = crypto:hash(sha, Stage1),
  Res = crypto:hash_final(
    crypto:hash_update(crypto:hash_update(crypto:hash_init(sha),Salt),Stage2)),
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
hash(S) when is_binary(S) ->
  hash(binary_to_list(S));
hash(S) -> hash(S, 1345345333, 16#12345671, 7).
hash([9 | S], N1, N2, Add) -> %% Skip tabs
  hash(S, N1, N2, Add);
hash([32 | S], N1, N2, Add) -> %% Skip spaces
  hash(S, N1, N2, Add);
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
  Val = trunc((float(NSeed1) / float(Mod))*31),
  rnd(N - 1, [Val | List], NSeed1, NSeed2).
