erespool
=====

An OTP application like episcina but with blackjack.

Erespool is designed as a pool for resources, with a variable amount of workers.
Each worker is a Pid.


Build
-----

    $ rebar3 compile


Usage
-----

Start Pool test_ers_pool
```
  Name = test_ers_pool,
  Args = #{
      cnum_min => 3,    %% Min workers
      cnum_max => 5,    %% Max workers
      free_min => 2,    %% Min Free workers when add new worker to pool
      conn_start_mfa => {ers_worker_emulator, start, []}, %% Start worker MFA ->{ok, Pid}
      conn_stop_mfa  => {ers_worker_emulator, stop,  []}  %% Stop worker MFA
    },
  ers:start(Name, Args).
```

Get worker pid
```
{ok, Pid} = ers:get_cons(test_ers_pool).
```

Return worker pid to pool
```
ers:ret_cons(test_ers_pool, Pid).
```

Stop pool
```
ers:stop(test_ers_pool).
```

See ers.erl for more command options.
