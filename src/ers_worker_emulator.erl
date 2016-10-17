
-module(ers_worker_emulator).

-behaviour(gen_server).

-export([start/0, stop/1]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).



-include("erespool.hrl").

-define(TIMEOUT, 40 * 1000).

start() -> 
  gen_server:start(?MODULE, [], []).

stop(Pid) ->
  gen_server:stop(Pid).

init([]) ->
  %?INF("Start", self()),
  {ok, #{}, ?TIMEOUT}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Gen Server api
handle_info(timeout, S)             -> {stop, normal, S};
handle_info(Msg, S)                 -> io:format("Unk msg ~p~n", [{self(), Msg}]), {noreply, S, ?TIMEOUT}.
code_change(_OldVersion, S, _Extra) -> {ok, S, ?TIMEOUT}.
terminate(_Reason, _S)              -> ok.

%%casts
handle_cast(Msg, S)                 -> io:format("Unk msg ~p~n", [{?p, Msg}]), {noreply, S, ?TIMEOUT}.
%%calls
handle_call(ping, _F, S)            -> {reply, pong, S, ?TIMEOUT};
handle_call({ping, N} ,_F, S)       -> timer:sleep(N), {reply, {pong, N}, S, ?TIMEOUT};
handle_call(Req,_From, S)           -> {reply, {err, unknown_command, ?p(Req)}, S, ?TIMEOUT}.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


