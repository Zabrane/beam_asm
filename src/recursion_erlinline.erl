-module(recursion_erlinline).
-export([ run/2
        ]).

-compile(inline).
-compile({inline, [run/2]}).
-compile({inline_size,   1000000}).
-compile({inline_effort, 1000000}).
-compile({inline_unroll,       3}).

run(A, B) when A < B ->
  run(B, A);
run(A, 0) when A >= 0 ->
  A;
run(A, B) when A >= 0, B >= 0 ->
  run(B, A rem B).
