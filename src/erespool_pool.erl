-module(erespool_pool).

-behaviour(gen_server).

-export([start/2, stop/1, start_link/2]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).


-export([get_conn/3, ret_conn/2, state/1, stat/1]).

-include("erespool.hrl").

start(Name, Args) ->
  supervisor:start_child(erespool_sup, [Name, Args]).

stop(Name) ->
  case is_pid(whereis(Name)) of
    true  -> gen_server:stop(Name);
    false -> ?e(pool_not_found)
  end.


start_link(Name, Args) when is_atom(Name) ->
  MandatoryArgsFields = [
    maps:get(cnum_min,            Args, u),
    maps:get(cnum_max,            Args, u),
    maps:get(free_min,            Args, u),
    maps:get(conn_start_mfa,      Args, u),
    maps:get(conn_stop_mfa,       Args, u)
  ],
  case lists:member(u, MandatoryArgsFields) of
    true  -> {err, {wrong_args, ?p}};
    false -> gen_server:start_link({local, Name}, ?MODULE, Args#{name => Name}, [])
  end.



init(Args = #{name := Name}) ->
  erlang:process_flag(trap_exit, true),

  S = #{
    name       => Name,
    conf       => Args,
    q          => [],
    conns      => [],
    orig_conns => [],
    ac         => 0    %% Add connects counter
  },

  InitS = try_add_conn(S),
  {ok, InitS}.

%
terminate(normal, #{orig_conns := []}) -> 
  ok;
terminate(normal, #{conf := Conf, orig_conns := OConns}) ->
  {M,F,A} = maps:get(conn_stop_mfa, Conf),
  [erlang:apply(M,F,[C|A]) || C <- OConns],
  ok;
terminate(R,S) ->
  ?INF("Unexpected terminate", {R, S}),
  ok.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Gen Server api
handle_info(timeout, S)             -> timeout_(S);
handle_info({'EXIT', Pid, _R}, S)   -> conn_exit_signal_(S, Pid);
handle_info({'DOWN', MRf,_,_,_},S)  -> client_down_signal(S, MRf);
handle_info(Msg, S)                 -> io:format("Unk msg ~p~n", [Msg]), {noreply, S, 0}.
code_change(_OldVersion, S, _Extra) -> {ok, S, 0}.

%%casts
handle_cast({ret_conn, Conn}, S)    -> ret_conn_(S, Conn);
handle_cast({add_conn, Conn}, S)    -> add_conn_(S, Conn);
handle_cast(Msg, S)                 -> io:format("Unk msg ~p~n", [{?p, Msg}]), {noreply, S, 0}.
%%calls
handle_call({get_conn,LT,T,P},F, S) -> get_conn_(F, S, LT, T, P);
handle_call(state, _F, S)           -> state_(S);
handle_call(stat, _F, S)            -> stat_(S);
handle_call(Req,_From, S)           -> {reply, {err, unknown_command, ?p(Req)}, S, 0}.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%
% {Time, ret, {Conn, MonitorRef}}       %% ret_conn queue elem
% {Time, get, {From, {LT, MonitorRef}}} %% get_coom queue elem
%

state(PoolName) ->
  gen_server:call(PoolName, state).
state_(S) ->
  {reply, S, S, 0}. 


stat(PoolName) ->
  gen_server:call(PoolName, stat).
stat_(S = #{q := Q, conns := Cs, orig_conns := OCs, conf := Conf}) ->
  Ret = #{
    conns           => length(OCs),
    free_conns      => length(Cs),
    queue_length    => length(Q),
    get_conn_queue  => length([get || {_,get,_}<- Q]),
    ret_conn_queue  => length([get || {_,ret,_}<- Q]),
    pool_conf       => Conf},
  {reply, Ret, S, 0}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% GET CONNECTION
get_conn(PoolName, LeaseTime, Timeout) ->
  case is_pid(whereis(PoolName)) of
    true  -> gen_server:call(PoolName, {get_conn, LeaseTime, Timeout, self()}, Timeout+100);
    false -> ?e(pool_not_found)
  end.

get_conn_(_From, S = #{conns := [Conn|Conns], q := Q}, LT, _T, ClientPid) ->
  MonitorRef = erlang:monitor(process, ClientPid),
  QElem = {?mnow + LT, ret, {Conn, MonitorRef}},
  NewQ = ordsets:add_element(QElem, Q),
  NewS = try_add_conn(S#{q := NewQ, conns := Conns}),
  {reply, {ok, Conn}, NewS, 0};

get_conn_(From, S = #{conns := [], q := Q}, LT, T, ClientPid) -> 
  MonitorRef = erlang:monitor(process, ClientPid),
  QElem = {?mnow + T, get, {From, {LT, MonitorRef}} },
  NewQ = ordsets:add_element(QElem, Q),
  NewS = try_add_conn(S#{q := NewQ}),
  {noreply, NewS, 0}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% RETURN CONNECTION
ret_conn(PoolName, Conn) ->
  case is_pid(whereis(PoolName)) of
    true  -> gen_server:cast(PoolName, {ret_conn, Conn});
    false -> ?e(pool_not_found)
  end.

ret_conn_(S = #{q := Q, conns := Conns}, Conn) ->
  FilterFun = fun
    ({_, ret, {C, M}}, {QAcc, ConnsAcc}) when C == Conn -> erlang:demonitor(M), {QAcc, [C|ConnsAcc]};
    (E, {QAcc, ConnsAcc}) -> {[E|QAcc], ConnsAcc}
  end,
  {NewQ, NewConns} = lists:foldr(FilterFun, {[], Conns}, Q),
  {noreply, S#{q := NewQ, conns := NewConns}, 0}.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% QEUES MANAGE
timeout_(S = #{q := Queue, conns := Conns}) ->
  Now = ?mnow,

  Qfun =
     fun
      (F, [{Time, get, {From, {_, M}} }|Q], Cs, QAcc) when Time  =< Now ->
          gen_server:reply(From, timeout),
          erlang:demonitor(M),
          F(F, Q, Cs, QAcc);
      (F, [{Time, ret, {C, M}}|Q], Cs, QAcc) when Time  =< Now -> 
          erlang:demonitor(M),
          ?INF("Conn not returned", {M, C}),
          F(F, Q, [C|Cs], QAcc); 
      (F, [{_Time, get, {From, {LT, M}} }|Q], [C|Cs], QAcc) ->
          gen_server:reply(From, {ok, C}),
          NewQ = ordsets:add_element({Now + LT, ret, {C, M}}, Q),
          F(F, NewQ, Cs, QAcc);
      (F, [E = {_, ret, _}|Q], Cs, QAcc) -> F(F, Q, Cs, [E|QAcc]);
      (_, [E = {_, get, _}|Q], [], QAcc) -> {[], lists:append(lists:reverse([E|QAcc]), Q)};
      (_, [], Cs, QAcc)                  -> {Cs, lists:reverse(QAcc)}
    end,

  {NewConns, NewQueue} = Qfun(Qfun, Queue, Conns, []),
  case NewQueue of
    [{T,_, _}|_] -> {noreply, S#{q := NewQueue, conns := NewConns}, T - Now};
    []           -> {noreply, S#{q := NewQueue, conns := NewConns}}
  end.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% MANAGE ADD CONNS
try_add_conn(S = #{conns := Conns, conf := Conf, orig_conns := OCs, ac := AC}) ->
  #{cnum_max := CnumMax, cnum_min := CnumMin, free_min := FreeMin} = Conf,
  ConnsLen      = length(OCs) + AC,
  FreeConnsLen  = length(Conns) + AC,
  case (ConnsLen < CnumMax andalso FreeConnsLen < FreeMin) orelse ConnsLen < CnumMin of
    true  -> add_conn(S), S#{ac := AC+1};
    false -> S
  end.


add_conn(#{conf := Conf}) ->
  Pid = self(),
  #{conn_start_mfa := {M,F,A}} = Conf,
  erlang:spawn( fun() -> 
    Res = 
      try
        erlang:apply(M,F,A)
      catch
        E:R -> ?e(E, R)
      end,
    gen_server:cast(Pid, {add_conn, Res}) 
  end).
 

add_conn_(S = #{conns := Conns, orig_conns := OConns, ac := AC}, {ok, Conn}) ->
  link(Conn),
  NewS = try_add_conn(S#{conns := [Conn|Conns], orig_conns := [Conn|OConns], ac := AC - 1}),
  {noreply, NewS, 0};
add_conn_(S = #{ac := AC}, Else) ->
  ?INF("Add conn error", Else),
  {noreply, S#{ac := AC - 1}, 0}.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% MANAGE CRASHED, DOWNED and EXITES CONNS AND CLIENTS
conn_exit_signal_(S = #{q := Q, conns := Conns, orig_conns := OConns}, Pid) ->
  NewQ = [E || E = {_, _, {Conn, _}} <- Q, Conn /= Pid],
  NewConns  = [C || C <- Conns,  C /= Pid],
  NewOConns = [C || C <- OConns, C /= Pid],
  NewS = try_add_conn(S#{q := NewQ, conns := NewConns, orig_conns := NewOConns}),
  {noreply, NewS, 0}.


client_down_signal(S = #{q := Q, conns := Conns}, MonitorRef) ->
  FilterFun = fun
    ({_, ret, {Conn, Ref}},       {AccQ, AccConns}) when Ref == MonitorRef -> {AccQ, [Conn|AccConns]};
    ({_, get, {_From,{_, Ref}} }, {AccQ, AccConns}) when Ref == MonitorRef -> {AccQ, AccConns};
    (E, {AccQ, AccConns}) -> {[E|AccQ], AccConns} 
  end,
  {NewQ, NewConns} = lists:foldr(FilterFun, {[], Conns}, Q),
  {noreply, S#{q := NewQ, conns := NewConns}, 0}. 
 

