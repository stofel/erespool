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

  supervisor:start_child(ers_sup, ChildPool).


stop_pool(Name) ->
  supervisor:terminate_child(ers_sup, Name).
  
  
