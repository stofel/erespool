%%%-------------------------------------------------------------------
%% @doc erespool top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module('erespool_sup').

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

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

    SupFlags = #{strategy    => simple_one_for_one,
                 intensity   => 5,   
                 period      => 10}, 

    ChildPool = #{id         => erespool_pool,
                  type       => worker,
                  start      => {erespool_pool, start_link, []},
                  modules    => [erespool_pool],
                  restart    => transient,
                  shutdown   => 2000},


    {ok, {SupFlags, [ChildPool]}}.


