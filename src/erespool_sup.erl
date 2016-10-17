%%%-------------------------------------------------------------------
%% @doc erespool top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module('erespool_sup').

-behaviour(supervisor).

%% API
-export([start_link/2]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link(Name, Args) ->
    supervisor:start_link(?MODULE, [Name, Args]).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% 
init([Name, Args]) ->

    SupFlags = #{strategy    => one_for_one,
                 intensity   => 5,
                 period      => 10},

    ChildPool = #{id         => Name,
                  type       => worker,
                  start      => {erespool, start_link, [Name, Args]},
                  modules    => [erespool],
                  restart    => transient,
                  shutdown   => 2000},

    {ok, {SupFlags, [ChildPool]}}.

