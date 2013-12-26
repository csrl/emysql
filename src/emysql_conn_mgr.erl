%% Copyright (c) 2009-2012
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
-module(emysql_conn_mgr).
-behaviour(gen_server).

-export([start_link/0]).
-export([
  add_pool/3, remove_pool/1, remove_all_pools/0,
  open_connections/2, close_connections/2,
  request_connection/2, renew_connection/1, release_connection/1
]).
-export([
  init/1, terminate/2, code_change/3,
  handle_call/3, handle_cast/2, handle_info/2
]).

-include("emysql_internal.hrl").

-record(state, {pools}).

%%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

%%------------------------------------------------------------------------------
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

%%------------------------------------------------------------------------------
add_pool(PoolId, Size, Config) ->
  case do_gen_call({add_pool, #pool{pool_id = PoolId, config = Config}}) of
    ok ->
      open_connections(PoolId, Size);
    Error ->
      Error
  end.

%%------------------------------------------------------------------------------
remove_pool(PoolId) ->
  case do_gen_call({remove_pool, PoolId}) of
    {ok, Pool} ->
      lists:foreach(
        fun (Pid) ->
          Pid ! {lock_connection_response, {error, pool_not_found}},
          ok
        end,
        queue:to_list(Pool#pool.waiting)),
      lists:foreach(
        fun emysql_conn:close/1, queue:to_list(Pool#pool.available)),
      lists:foreach(
        fun emysql_conn:close/1, gb_trees:values(Pool#pool.locked)),
      ok;
    Error ->
      Error
  end.

%%------------------------------------------------------------------------------
remove_all_pools() ->
  {ok, PoolIds} = do_gen_call(pools),
  lists:foreach(fun remove_pool/1, PoolIds).

%%------------------------------------------------------------------------------
open_connections(PoolId, Count) ->
  %% Potential race condition if the pool is removed and readded with
  %% a new configuration before the new connections are added into the
  %% pool. However, this should be acceptable as it would be the
  %% caller's responsibility to properly manage its pool usage. We do
  %% not want to lock the Pool from use while adding new connections.
  case do_gen_call({get_config, PoolId}) of
    {ok, PoolConfig} ->
      OpenConn = fun(_, Connections) ->
        case open_connection(PoolId, PoolConfig) of
          {ok, Connection} ->
            [Connection | Connections];
          Error ->
            lists:foreach(fun emysql_conn:close/1, Connections),
            throw(Error)
        end
      end,
      try lists:foldl(OpenConn, [], lists:seq(1, Count)) of
        Conns ->
          case do_gen_call({add_connections, PoolId, Conns}) of
            ok ->
              ok;
            Error ->
              lists:foreach(fun emysql_conn:close/1, Conns),
              Error
          end
      catch
        Error ->
          Error
      end;
    Error ->
      Error
  end.

%%------------------------------------------------------------------------------
close_connections(PoolId, Count) ->
  case do_gen_call({remove_connections, PoolId, Count}) of
    {ok, Connections} ->
      lists:foreach(fun emysql_conn:close/1, Connections),
      ok;
    Error ->
      Error
  end.

%%------------------------------------------------------------------------------
request_connection(PoolId, 0) ->
  do_gen_call({lock_connection, PoolId, false});

request_connection(PoolId, Timeout)->
  %% Try to lock a connection. if no connections are available then
  %% wait to be notified of the next available connection.
  case do_gen_call({lock_connection, PoolId, true}) of
    queued ->
      receive
        {lock_connection_response, Response} -> Response
      after Timeout ->
        do_gen_call({abort_wait, PoolId}),
        %% Handle race condition of connection becoming available just
        %% as we timed out and before we aborted the wait.
        receive
          {lock_connection_response, Response} -> Response
        after 0 ->
          unavailable
        end
      end;
    Response ->
      Response
  end.

%%------------------------------------------------------------------------------
renew_connection(Connection) ->
  emysql_conn:close(Connection),
  PoolId = Connection#connection.pool_id,
  case do_gen_call({get_config, PoolId}) of
    {ok, PoolConfig} ->
      case open_connection(PoolId, PoolConfig) of
        {ok, NewConnection} ->
          ok = do_gen_call({replace_connection, Connection, NewConnection}),
          {ok, NewConnection};
        Error ->
          Error
      end;
    Error ->
      Error
  end.

%%------------------------------------------------------------------------------
release_connection(Connection) ->
  do_gen_call({unlock_connection, Connection}).

%%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

%%------------------------------------------------------------------------------
do_gen_call(Msg) ->
  gen_server:call(?MODULE, Msg, infinity).

%%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

%%------------------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%------------------------------------------------------------------------------
init([]) ->
  {ok, #state{pools = load_pools(emysql_app:config(pools))}}.

%%------------------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is
%% about to terminate. It should be the opposite of Module:init/1 and
%% do any necessary cleaning up. When it returns, the gen_server
%% terminates with Reason. The return value is ignored.
%%------------------------------------------------------------------------------
terminate(_Reason, _State) ->
  ok.

%%------------------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%------------------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%------------------------------------------------------------------------------
%% Function: handle_call(Request, From, State) ->
%%                                  {reply, Reply, State} |
%%                                  {reply, Reply, State, Timeout} |
%%                                  {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, Reply, State} |
%%                                  {stop, Reason, State}
%% Description: Handling call messages
%%------------------------------------------------------------------------------
handle_call(pools, _From, State) ->
  {reply, {ok, [Pool#pool.pool_id || Pool <- State#state.pools]}, State};

handle_call({get_config, PoolId}, _From, State) ->
  case find_pool(PoolId, State#state.pools) of
    {Pool, _OtherPools} ->
      {reply, {ok, Pool#pool.config}, State};
    undefined ->
      {reply, {error, pool_not_found}, State}
  end;

handle_call({add_pool, Pool}, _From, State) ->
  case find_pool(Pool#pool.pool_id, State#state.pools) of
    {_, _} ->
      {reply, {error, pool_already_exists}, State};
    undefined ->
      {reply, ok, State#state{pools = [Pool|State#state.pools]}}
  end;

handle_call({remove_pool, PoolId}, _From, State) ->
  case find_pool(PoolId, State#state.pools) of
    {Pool, OtherPools} ->
      {reply, {ok, Pool}, State#state{pools=OtherPools}};
    undefined ->
      {reply, {error, pool_not_found}, State}
  end;

handle_call({add_connections, PoolId, Conns}, _From, State) ->
  case find_pool(PoolId, State#state.pools) of
    {Pool, OtherPools} ->
      Pool1 = Pool#pool{
        size = Pool#pool.size + length(Conns),
        available = queue:join(queue:from_list(Conns), Pool#pool.available)},
      State1 = State#state{
        pools = [serve_waiting_pids(Pool1)|OtherPools]},
      {reply, ok, State1};
    undefined ->
      {reply, {error, pool_not_found}, State}
  end;

handle_call({remove_connections, PoolId, Num}, _From, State) ->
  case find_pool(PoolId, State#state.pools) of
    {Pool, OtherPools} ->
      case Num > queue:len(Pool#pool.available) of
        true ->
          State1 = State#state{pools = [Pool#pool{available = queue:new()}]},
          {reply, {ok, queue:to_list(Pool#pool.available)}, State1};
        false ->
          {Conns, OtherConns} = queue:split(Num, Pool#pool.available),
          State1 = State#state{
            pools = [Pool#pool{available = OtherConns} | OtherPools]},
          {reply, {ok, queue:to_list(Conns)}, State1}
      end;
    undefined ->
      {reply, {error, pool_not_found}, State}
  end;

handle_call({lock_connection, PoolId, Wait}, {From, _Mref}, State) ->
  case find_pool(PoolId, State#state.pools) of
    {Pool, OtherPools} ->
      case lock_next_connection(Pool) of
        {ok, Connection, PoolNow} ->
          {reply, {ok, Connection}, State#state{pools=[PoolNow|OtherPools]}};
        unavailable when Wait =:= true ->
          %% place the calling pid at the end of the waiting queue of its pool
          PoolNow = Pool#pool{waiting = queue:in(From, Pool#pool.waiting)},
          {reply, queued, State#state{pools=[PoolNow|OtherPools]}};
        unavailable when Wait =:= false ->
          {reply, unavailable, State}
      end;
    undefined ->
      {reply, {error, pool_not_found}, State}
  end;

handle_call({abort_wait, PoolId}, {From, _Mref}, State) ->
  case find_pool(PoolId, State#state.pools) of
    {Pool, OtherPools} ->
      %% Remove From from the wait queue
      QueueNow = queue:filter(fun(Pid) -> Pid =/= From end, Pool#pool.waiting),
      PoolNow = Pool#pool{waiting = QueueNow},
      {reply, ok, State#state{pools=[PoolNow|OtherPools]}};
    undefined ->
      {reply, {error, pool_not_found}, State}
  end;

handle_call({unlock_connection, Conn}, _From, State) ->
  case find_pool(Conn#connection.pool_id, State#state.pools) of
    {Pool, OtherPools} ->
      Pool1 = Pool#pool{
        available = queue:in(
          Conn#connection{locked_at=undefined}, Pool#pool.available),
        locked = gb_trees:delete_any(
          Conn#connection.socket, Pool#pool.locked)},
      {reply, ok, State#state{pools=[serve_waiting_pids(Pool1)|OtherPools]}};
    undefined ->
      {reply, {error, pool_not_found}, State}
  end;

handle_call({replace_connection, OldConn, NewConn}, _From, State) ->
  %% replace an existing locked connection with the newly supplied one
  %% and keep it in the locked list so that the caller can continue to use it
  %% without having to lock another connection.
  case find_pool(OldConn#connection.pool_id, State#state.pools) of
    {Pool, OtherPools} ->
      Locked = gb_trees:delete_any(
        OldConn#connection.socket, Pool#pool.locked),
      Pool1 = Pool#pool{
        locked = gb_trees:enter(
          NewConn#connection.socket, connection_locked_at(NewConn), Locked)},
      {reply, ok, State#state{pools=[Pool1|OtherPools]}};
    undefined ->
      {reply, {error, pool_not_found}, State}
  end;

handle_call(_, _From, State) -> {reply, {error, invalid_call}, State}.

%%------------------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%------------------------------------------------------------------------------
handle_cast(_Msg, State) ->
  {noreply, State}.

%%------------------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%------------------------------------------------------------------------------
handle_info(_Info, State) ->
  {noreply, State}.

%%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%%% Internal functions
%%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

%%------------------------------------------------------------------------------
load_pools([]) ->
  [];

load_pools([{PoolId, Props} | Pools]) ->
  {Size, Config} = emysql_app:pool_config(Props),
  Conns = init_connections(PoolId, Config, Size),
  Pool = #pool{
    pool_id = PoolId,
    config = Config,
    size = length(Conns),
    available = queue:from_list(Conns)},
  [Pool | load_pools(Pools)].

%%------------------------------------------------------------------------------
init_connections(PoolId, Config, Count) ->
  lists:foldl(
    fun(_, Conns) ->
      case open_connection(PoolId, Config) of
        {ok, Conn} ->
          [Conn | Conns];
        _ ->
          Conns
      end
    end,
    [],
    lists:seq(1, Count)).

%%------------------------------------------------------------------------------
open_connection(PoolId, #pool_config{
    host=Host, port=Port, user=User, password=Pass,
    database=Db, collation=Co}) ->
  try emysql_conn:open(User, Pass, Host, Port, Db, Co) of
    Connection ->
      case give_manager_control(Connection#connection.socket) of
        ok ->
          {ok, Connection#connection{pool_id = PoolId}};
        {error, _Reason} = Error ->
          emysql_conn:close(Connection),
          Error
      end
  catch
    {_Who, Error} ->
      {error, Error}
  end.

%%------------------------------------------------------------------------------
give_manager_control(Sock) ->
  case whereis(?MODULE) of
    undefined -> {error, manager_not_found};
    MgrPid -> gen_tcp:controlling_process(Sock, MgrPid)
  end.

%%------------------------------------------------------------------------------
find_pool(PoolId, Pools) ->
  find_pool(PoolId, Pools, []).

find_pool(_, [], _) -> undefined;

find_pool(PoolId, [#pool{pool_id = PoolId} = Pool|Tail], OtherPools) ->
  {Pool, lists:append(OtherPools, Tail)};

find_pool(PoolId, [Pool|Tail], OtherPools) ->
  find_pool(PoolId, Tail, [Pool|OtherPools]).

%%------------------------------------------------------------------------------
lock_next_connection(Pool) ->
  case lock_next_connection(Pool#pool.available, Pool#pool.locked) of
    {ok, Connection, OtherAvailable, NewLocked} ->
      {ok, Connection, Pool#pool{available=OtherAvailable, locked=NewLocked}};
    unavailable ->
      unavailable
  end.

lock_next_connection(Available, Locked) ->
  case queue:out(Available) of
    {{value, Conn}, OtherAvailable} ->
      NewConn = connection_locked_at(Conn),
      NewLocked = gb_trees:enter(NewConn#connection.socket, NewConn, Locked),
      {ok, NewConn, OtherAvailable, NewLocked};
    {empty, _} ->
      unavailable
  end.

%%------------------------------------------------------------------------------
connection_locked_at(Conn) ->
  Conn#connection{locked_at=lists:nth(2, tuple_to_list(now()))}.

%%------------------------------------------------------------------------------
serve_waiting_pids(Pool) ->
  {Waiting, Available, Locked} = serve_waiting_pids(
    Pool#pool.waiting, Pool#pool.available, Pool#pool.locked),
  Pool#pool{waiting=Waiting, available=Available, locked=Locked}.

serve_waiting_pids(Waiting, Available, Locked) ->
  case queue:is_empty(Waiting) of
    false ->
      case lock_next_connection(Available, Locked) of
        {ok, Connection, OtherAvailable, NewLocked} ->
          {{value, Pid}, OtherWaiting} = queue:out(Waiting),
          erlang:send(Pid, {lock_connection_response, {ok, Connection}}),
          serve_waiting_pids(OtherWaiting, OtherAvailable, NewLocked);
        unavailable ->
          {Waiting, Available, Locked}
      end;
    true ->
      {Waiting, Available, Locked}
  end.
