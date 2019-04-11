-module(recursion_erl).
-export([ run/2
        ]).

run(A, B) when A < B ->
  run(B, A);
run(A, 0) when A >= 0 ->
  A;
run(A, B) when A >= 0, B >= 0 ->
  run(B, A rem B).
