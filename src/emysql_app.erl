%% Copyright (c) 2009
%% Bill Warnecke <bill@rupture.com>
%% Jacob Vorreuter <jacob.vorreuter@gmail.com>
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
-module(emysql_app).
-behaviour(application).

-export([start/2, stop/1]).
-export([config/1, pool_config/1, modules/0]).

-include("emysql_internal.hrl").

%%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

%%------------------------------------------------------------------------------
start(_Type, _StartArgs) ->
  emysql_sup:start_link().

%%------------------------------------------------------------------------------
stop(_State) ->
  emysql_conn_mgr:remove_all_pools(),
  ok.

%%------------------------------------------------------------------------------
config(pools) ->
  case application:get_env(emysql, pools) of
    {ok, Pools} when is_list(Pools) ->
      Pools;
    _ ->
      []
  end.

%%------------------------------------------------------------------------------
pool_config(Props) ->
  Size = proplists:get_value(size, Props, 1),
  User = proplists:get_value(user, Props),
  Password = proplists:get_value(password, Props),
  Host = proplists:get_value(host, Props, "localhost"),
  Port = proplists:get_value(port, Props, 3306),
  Database = proplists:get_value(database, Props, ""),
  Collation = proplists:get_value(collation, Props),
  verify_pool_config(Size, User, Password, Host, Port, Database, Collation),
  {Size, #pool_config{
    user = User,
    password = Password,
    host = Host,
    port = Port,
    database = Database,
    collation = Collation}}.
verify_pool_config(Size, User, Password, Host, Port, Database, Collation) when
    is_integer(Size) andalso Size >= 0,
    is_binary(User) orelse is_list(User),
    is_binary(Password) orelse is_list(Password),
    is_list(Host) orelse is_atom(Host) orelse is_tuple(Host),
    is_number(Port) andalso Port >= 0,
    is_binary(Database) orelse is_list(Database),
    undefined =:= Collation orelse is_integer(Collation) andalso
      Collation >= 1 andalso Collation =< 255 ->
  ok.

%%------------------------------------------------------------------------------
modules() ->
  {ok, Modules} = application_controller:get_key(emysql, modules),
  Modules.
