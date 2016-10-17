
-module(erespool_misc).
-export([c_r/2, milliseconds/0]).
-export([random_int/1, random_int/2]).


% recursion for function list
c_r(FunList, Args) ->
  case_recursion(FunList, Args).
case_recursion(FunList, Args) ->
  Fun = fun
            (F, [N|R], Acc = #{status := ok}) -> F(F, R, apply(N, [Acc]));
            (_F, _,    Acc = #{status := done}) -> Acc#{status := ok};
            (_F, _,    Acc) -> Acc
        end,
  Fun(Fun, FunList, Args).


milliseconds() ->
  {Me, S, M} = os:timestamp(),
  (Me*1000000 + S)*1000 + round(M/1000).
  
%
random_int(1) -> 1;
random_int(N) ->
  {A,B,C} = erlang:timestamp(),
  random:seed(A,B,C),
  random:uniform(N).
random_int(S, T) when S > 0, T > 0, T > S ->
  {A,B,C} = erlang:timestamp(),
  random:seed(A,B,C),
  random:uniform(T-S+1)+S-1.

