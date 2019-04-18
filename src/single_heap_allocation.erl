-module(single_heap_allocation).
-export([ run/1
        ]).

run(Year) ->
  case calendar:is_leap_year(Year) of
    true ->
      {0.0, 0.0, 0.0};
    false ->
      {rand:normal(), rand:normal(), rand:normal()}
  end.
