-module(reftrick_erl).
-export([ run/2
        ]).

run(Pid1, Pid2) ->
  Mon1 = monitor(process, Pid1),
  Mon2 = monitor(process, Pid2),
  Pid1 ! {request, Mon1, self()},
  Pid2 ! {request, Mon1, self()},
  receive
    {response, Mon1, Val} ->
      {ok, Val};
    {'DOWN', Mon1, process, Pid1, Reason} ->
      {error, Reason};
    {'DOWN', Mon2, process, Pid2, Reason} ->
      {error, Reason}
  end.
