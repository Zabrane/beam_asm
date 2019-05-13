-module(recursion_test).

-behaviour(perftest).
-export([ args/0
        , setup/1
        , validate/3
        , cleanup/1
        ]).

args() ->
  #{ gcd => [17]
   , iter => [500, 400, 300, 200, 100, 50, 5]
   }.

setup(#{gcd := GCD, iter := Iter}) when Iter > 0 ->
  module_loaded(recursion_asmopt) orelse load_and_hack(recursion_asmopt),
  {A, B} = generate(GCD, 0, Iter),
  {ok, [B, A], GCD}.

validate(normal, GCD, GCD) ->
  true;
validate(_, _, _) ->
  false.

cleanup(_) ->
  ok.

generate(A, B, 0) ->
  {A, B};
generate(A, B, N) ->
  generate(B + A * 2, A, N - 1).

load_and_hack(Mod) ->
  File = code:where_is_file(atom_to_list(Mod) ++ ".beam"),
  {ok, Mod, Chunks} = beam_lib:all_chunks(File),
  NewChunks = lists:map(fun ({"Code", Code}) -> {"Code", hack(Code)}; (Chunk) -> Chunk end,
                        Chunks),
  {ok, Beam} = beam_lib:build_module(NewChunks),
  {module, Mod} = code:load_binary(Mod, File, Beam),
  ok.

%% This hack doesn't work as intended: we can jump to label @ordered,
%% but not to @regular. The latter would take us to @divide for some
%% reason. Probably the beam's internal state gets a bit messed up
%% when loading this hacked module?
hack(Code) ->
  [P1, P2, P3] = binary:split(Code, <<16#062025:24>>, [global]),
  <<P1/binary, 16#062035:24, P2/binary, 16#062035:24, P3/binary>>.
