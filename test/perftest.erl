-module(perftest).

%% API
-export([ start/0
        , start/1
        , module/1
        ]).

%% Metadata about arguments for a test case: a map from the name of
%% the arguments to the list of possible values to test with.
%%
%% The test case will be executed will all the possible combination of
%% the arguments.
-type test_args_meta() :: #{Arg :: atom() => Values :: nonempty_list()}.

%% A specific combination of arguments selected from a
%% `test_args_meta' structure.
-type test_args() :: #{Arg :: atom() => Value :: term()}.

%% Test case state returned from the setup function to be passed to
%% the cleanup.
-type setup_state() :: term().

%% The possible return types of a function: normal return or various
%% exceptions.
-type return_type() ::  normal | throw | error | exit.

-export_type([ test_args_meta/0
             , test_args/0
             , setup_state/0
             , return_type/0
             ]).

%% ---------------------------------------------------------------------
%% Behaviour callbacks
%% ---------------------------------------------------------------------

%% Provide metadata about arguments for the test case.
-callback args() -> test_args_meta().

%% Prepare for executing the test case with the given arguments. The
%% function shall return the list of arguments to pass to the actual
%% test case and the state to pass to the `cleanup' function after
%% executing the test.
-callback setup(test_args()) -> {ok, list(), setup_state()}.

%% Validate the return value from a test case.
-callback validate(return_type(), term(), setup_state()) -> boolean().

%% Clean up after running a test case.
-callback cleanup(setup_state()) -> term().

%% ---------------------------------------------------------------------
%% API
%% ---------------------------------------------------------------------

%% @doc Execute all test modules found in the same directory with this
%% module.
-spec start() -> ok.
start() ->
  start(find_mods("*_test")).

%% @doc Execute a specific list of test modules.
-spec start([module()]) -> ok.
start(Mods) ->
  lists:foreach(fun module/1, Mods).

%% @doc Execute a single test module.
-spec module(module()) -> ok.
module(Mod) ->
  Args = Mod:args(),
  ArgsList = maps:to_list(Args),
  ImplMods = find_mods(impl_pattern(Mod)) -- [Mod],
  [test(Mod, ImplMod, GeneratedArgs)
   || GeneratedArgs <- generate_args(ArgsList),
      ImplMod <- ImplMods
  ],
  ok.

%% ---------------------------------------------------------------------
%% Internal helper functions
%% ---------------------------------------------------------------------

%% @doc Find modules in the same directory as this one that match the
%% given pattern.
-spec find_mods(string()) -> [module()].
find_mods(Pattern) ->
  Dir = filename:dirname(code:which(?MODULE)),
  Ext = code:objfile_extension(),
  [list_to_atom(filename:basename(File, Ext))
   || File <- filelib:wildcard(Pattern ++ Ext, Dir)
  ].

%% @doc Get the module name pattern for implementation of a test case
%% module. A test case module must have a name like `X_test', and the
%% corresponding implementations will have to match the `X_*' pattern.
-spec impl_pattern(module() | string()) -> string().
impl_pattern(Mod) when is_atom(Mod) ->
  impl_pattern(atom_to_list(Mod));
impl_pattern("_test") ->
  "_*";
impl_pattern([C | Cs]) ->
  [C | impl_pattern(Cs)].

%% @doc Generate all combination of arguments from argument meta-data.
-spec generate_args([{K, [V]}]) -> [#{K => V}].
generate_args([]) ->
  [#{}];
generate_args([{Arg, Values} | Args]) ->
  [maps:put(Arg, Value, GeneratedArgs)
   || Value <- Values,
      GeneratedArgs <- generate_args(Args)
  ].

test(Mod, ImplMod, Args) ->
  {module, ImplMod} = code:ensure_loaded(ImplMod),
  erlang:system_flag(schedulers_online, 1),
  timer:sleep(100),
  erlang:system_flag(schedulers_online, erlang:system_info(schedulers)),
  process_flag(scheduler, 1),
  N = 1000,
  Warmup = 100,
  Times = lists:sort(
            lists:nthtail(
              Warmup,
              test_loop(Mod, ImplMod, Args, N + Warmup))),
  Median = lists:nth(N div 2, Times),
  Mean = lists:sum(Times) / N,
  Dev = math:sqrt(
          lists:sum([(Time - Mean) * (Time - Mean) || Time <- Times])
          / (N - 1)),
  io:format("median: ~7b ns mean: ~10.3f +- ~10.3f ns [~w] ~w~n",
            [Median, Mean, Dev, ImplMod, Args]),
  process_flag(scheduler, 0),
  ok.

test_loop(Mod, ImplMod, Args, N) when N > 0 ->
  S = self(),
  P = spawn_link(fun () -> S ! {self(), test_proc(Mod, ImplMod, Args)} end),
  receive {P, Time} ->
      [Time | test_loop(Mod, ImplMod, Args, N - 1)]
  end;
test_loop(_, _, _, 0) ->
  [].

test_proc(Mod, ImplMod, Args) ->
  process_flag(scheduler, erlang:system_info(schedulers)),
  process_flag(priority, max),
  {ok, FunArgs, State} = Mod:setup(Args),
  erlang:yield(),
  T1 = os:perf_counter(),
  {Type, Value, T2} =
    try apply(ImplMod, run, FunArgs) of
      V ->
        PC = os:perf_counter(),
        {normal, V, PC}
    catch
      T:V ->
        PC = os:perf_counter(),
        {T, V, PC}
    end,
  true = Mod:validate(Type, Value, State),
  Mod:cleanup(State),
  erlang:convert_time_unit(T2 - T1, perf_counter, nanosecond).
