
%% Point
-define(p,         list_to_binary(io_lib:format("Mod:~w line:~w", [?MODULE,?LINE]))).
-define(p(Reason), list_to_binary(io_lib:format("Mod:~w line:~w ~100P", [?MODULE,?LINE, Reason, 300]))).

-define(e(ErrCode), {err, {ErrCode, ?p}}).
-define(e(ErrCode, Reason), {err, {ErrCode, ?p(Reason)}}).

% NOW time in seconds
-define(now, erlang:system_time(seconds)).
-define(mnow, ers:milliseconds()).

%% Log messages
-define(INF(Str, Term), io:format("ERESPOOL INFO LOG: ~p:~p ~p ~100P~n", [?MODULE, ?LINE, Str, Term, 300])).
