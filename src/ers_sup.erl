%%%-------------------------------------------------------------------
%% @doc erespool top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module('ers_sup').

-behaviour(supervisor).

%% API
-export([start_link/0, start_pool/2, stop_pool/1]).

%% Supervisor callbacks
-export([init/1]).

-include("../include/erespool.hrl").


-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% 
init([]) ->

    SupFlags = #{strategy    => one_for_one,
                 intensity   => 5,   
                 period      => 10}, 

    {ok, {SupFlags, []}}.


start_pool(Name, Args) ->
  ChildPool = #{id         => Name,
                type       => supervisor,
                start      => {erespool_sup, start_link, [Name, Args]},
                modules    => [erespool_sup],
                restart    => temporary,
                shutdown   => 2000},

  case supervisor:start_child(?MODULE, ChildPool) of
    {ok, _Pid} -> {ok, erlang:whereis(Name)};
    Else -> Else
  end.


stop_pool(Pid) when is_pid(Pid) ->
  Name = gen_server:call(Pid, name),
  stop_pool(Name);

stop_pool(Name) ->
  case supervisor:get_childspec(?MODULE, Name) of
    {ok, #{id := Id}}  -> supervisor:terminate_child(?MODULE, Id);
    {error, not_found} -> ?e(no_such_pool);
    Else               -> ?e(unknown_error, Else)
  end.

