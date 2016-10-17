-module(ers).

-export([start/2, stop/1]).
-export([get_conn/1, get_conn/2, get_conn/3, ret_conn/2]).
-export([list/0, stat/1, state/1]).


% tests
-export([t_start/0, t_stop/0, t/1]).

-include("erespool.hrl").

-type err() :: {err, {atom(), binary()}}. 


%% @doc Start the pool
%% Args = #{
%%      cnum_min => ConnMin,
%%      cnum_max => ConnMax,
%%      free_min => FreeConnMin,
%%      conn_start_mfa => {StartM, StartF, StartA},
%%      conn_stop_mfa  => {StopM, StopF, StopA}
%% }
%% ConnMin, ConnMax, FreeConnMin - non negative integers, be careful 
%%
%% conn_start_mfa should be:
%% erlang:apply(StartM, StartF, StartA) -> {ok, pid()}
%%
%% conn_stop_mfa should be:
%% erlang:apply(StopM, StopF, StopA) -> ok.
%%
-spec start(atom(), map()) -> {ok, pid}|err()|{error, supevisor:startchild_err()}.
start(Name, Args) -> 
  erespool_pool:start(Name, Args).


%% @doc Stop the pool, close all workers
-spec stop(atom()) -> ok|err().
stop(Name) ->
  erespool_pool:stop(Name).


%% @doc Get a worker pid on 10 seconds lease, 
%%      wait at most 3 seconds before giving up.
-spec get_conn(atom()) -> {ok, pid()}|timeout|err().
get_conn(PoolName) ->
  get_conn(PoolName, 10000). 

%% @doc Get a worker pid on LeaseTime milliseconds lease, 
%%      wait at most 3 seconds before giving up.
-spec get_conn(atom(), integer()) -> {ok, pid()}|timeout|err().
get_conn(PoolName, LeaseTime) ->
  get_conn(PoolName, LeaseTime, 3000).

%% @doc Get a worker pid on LeaseTime milliseconds lease, 
%%      wait at most Timeout milliseconds before giving up.
-spec get_conn(atom(), integer(), integer()) -> {ok, pid()}|timeout|err().
get_conn(PoolName, LeaseTime, Timeout) ->
  erespool_pool:get_conn(PoolName, LeaseTime, Timeout).


%% @doc Retrun worker pid to the pool
-spec ret_conn(atom(), pid()) -> ok|err().
ret_conn(PoolName, Conn) -> 
  erespool_pool:ret_conn(PoolName, Conn).
  


%% Misc
%% @doc List of pools
-spec list() -> list().
list() ->
  F = fun(Pid) -> 
    {registered_name, Name} = erlang:process_info(Pid, registered_name),
    Name
  end,
  [F(Pid) || {_,Pid,_,_} <- supervisor:which_children(erespool_sup)].

%% @doc Current pool state
-spec stat(atom()) -> map().
stat(PoolName) ->
  erespool_pool:stat(PoolName).

%% @doc Current pool state
-spec state(atom()) -> map().
state(PoolName) ->
  erespool_pool:state(PoolName).



%% Tests
t_start() -> 
  Name = test_ers_pool,
  Args = #{
      cnum_min => 3,
      cnum_max => 5,
      free_min => 2,
      conn_start_mfa => {ers_worker_emulator, start, []},
      conn_stop_mfa  => {ers_worker_emulator, stop,  []}  
    },
  start(Name, Args).

t_stop() ->
  Name = test_ers_pool,
  stop(Name).
  
t(1) ->
  Name = test_ers_pool,
  erespool_pool:list(Name);

t(2) ->
  Name = test_ers_pool,
  case get_conn(Name) of
    {ok, Conn} -> gen_server:call(Conn, ping), {ok, Conn};
    Else -> 
      ?INF("get_conn error", Else),
      erlang:error(crash)
  end;

t(3) ->
   Name = test_ers_pool,
   get_conn(Name);

t(4) ->
  t_start(),
  ?INF("t_start", []),
  t({4, 10}),
  t_stop(),
  ?INF("t_stop", []);

t({4, N}) when N > 0 ->
  {ok, Conn} = get_conn(test_ers_pool),
  Res = gen_server:call(Conn, ping),
  ?INF("Ping", [Res]),
  ret_conn(test_ers_pool, Conn),
  t({4, N-1});

t({4, N}) -> N;

%
t(5) ->
  spawn(fun() ->
  case get_conn(test_ers_pool, 5000, 15000) of
    {ok, Conn} ->
      Res = gen_server:call(Conn, ping),
      ?INF("Ping", [Res]),
      %ret_conn(test_ers_pool, Conn);
      case erespool_misc:random_int(2) > 1 of
        true -> ret_conn(test_ers_pool, Conn);
        false -> 
          timer:sleep(6050),
          ret_conn(test_ers_pool, Conn)
      end;
    Else -> ?INF("Else", [Else])
  end, 
  timer:sleep(1)
  end);

t(6) ->
  ?INF("Pid", self()).




 


  
