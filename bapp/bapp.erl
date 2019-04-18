-module(bapp).
-export([ main/1
        , format_error/1
        ]).

main([File]) ->
  main(["-o", ".", File]);
main(["-o", WritePath, File]) ->
  Steps = [ fun read_file/1
          , fun tokenize/1
          , fun parse/1
          , fun add_module_info/1
          , fun unique_labels/1
          , fun mark_function_labels/1
          , fun add_func_infos/1
          , fun number_labels/1
          , fun to_asm_terms/1
          , fun lay_out/1
          , fun write_file/1
          ],
  State = #{ file => File
           , module => list_to_atom(filename:basename(File, ".bapp"))
           , write_path => WritePath
           },
  do_steps(Steps, State);
main(_) ->
  io:format(standard_error, "Usage: ~s FILE~n", [escript:script_name()]),
  halt(1).

read_file(State = #{file := File}) ->
  case file:read_file(File) of
    {ok, Bin} ->
      {ok, State#{code => Bin}};
    {error, Error} ->
      {error, {file, Error}}
  end.

tokenize(State = #{code := Bin}) ->
  case bapp_lexer:string(unicode:characters_to_list(Bin)) of
    {ok, Tokens, EndLine} ->
      {ok, State#{code => Tokens ++ [{'$end', EndLine}]}};
    {error, Error, _} ->
      {error, Error}
  end.

parse(State = #{code := Tokens}) ->
  case bapp_parser:parse(Tokens) of
    {ok, #{exports := Exports, funs := Funs}} ->
      {ok, State#{code => Funs, exports => Exports}};
    {error, _} = Error ->
      Error
  end.

add_module_info(State = #{module := Mod, exports := Exports, code := Code}) ->
  {ok, State#{ exports => [{module_info, 0}, {module_info, 1}] ++ Exports
             , code => Code ++ [ { {module_info, 0}
                                 , [ {op, move, [Mod, {register, {x,0}}]}
                                   , {op, call_ext_only, [fun erlang:get_module_info/1]}
                                   ]}
                               , { {module_info, 1}
                                 , [ {op, move, [{register, {x,0}}, {register, {x,1}}]}
                                   , {op, move, [Mod, {register, {x,0}}]}
                                   , {op, call_ext_only, [fun erlang:get_module_info/2]}
                                   ]}
                               ]}}.

unique_labels(State = #{code := Code}) ->
  case unique_labels_fun_fold(Code, #{}, []) of
    {ok, NewCode, Labels} ->
      {ok, State#{code => NewCode, labels => Labels}};
    {error, _} = Error ->
      Error
  end.

unique_labels_fun_fold([], Labels, Acc) ->
  {ok, lists:reverse(Acc), Labels};
unique_labels_fun_fold([{_FunName, [{label, Label} | Ops]} = Fun | Funs], Labels, Acc) ->
  case maps:is_key(Label, Labels) of
    true ->
      make_error("label ~p is already defined", [Label]);
    false ->
      case unique_labels_ops_fold(Ops, Labels#{Label => #{function => true}}) of
        {ok, NewLabels} ->
          unique_labels_fun_fold(Funs, NewLabels, [Fun | Acc]);
        {error, _} = Error ->
          Error
      end
  end;
unique_labels_fun_fold([{FunName, Ops} | Funs], Labels, Acc) ->
  Label = make_ref(),
  case unique_labels_ops_fold(Ops, Labels#{Label => #{function => true}}) of
    {ok, NewLabels} ->
      unique_labels_fun_fold(Funs, NewLabels, [{FunName, [{label, Label} | Ops]} | Acc]);
    {error, _} = Error ->
      Error
  end.

unique_labels_ops_fold([], Labels) ->
  {ok, Labels};
unique_labels_ops_fold([{label, Label} | Ops], Labels) ->
  case maps:is_key(Label, Labels) of
    true ->
      make_error("label ~p is already defined", [Label]);
    false ->
      unique_labels_ops_fold(Ops, Labels#{Label => #{function => false}})
  end;
unique_labels_ops_fold([_ | Ops], Labels) ->
  unique_labels_ops_fold(Ops, Labels).

mark_function_labels(State = #{code := Code, labels := Labels}) ->
  NewLabels = lists:foldl(fun mark_function_labels_fun_fold/2, Labels, Code),
  {ok, State#{labels => NewLabels}}.

mark_function_labels_fun_fold({_FunName, Ops}, Labels) ->
  lists:foldl(fun mark_function_labels_ops_fold/2, Labels, Ops).

mark_function_labels_ops_fold({op, Call, [_N, {label, Label} | _]}, Labels)
  when Call =:= call;
       Call =:= call_only;
       Call =:= call_last ->
  maps:update_with(Label, fun (Info) -> Info#{function => true} end, Labels);
mark_function_labels_ops_fold(_Other, Labels) ->
  Labels.

add_func_infos(State = #{module := Mod, code := Code, labels := Labels}) ->
  {NewCode, {NewLabels, _Mod}} =
    lists:mapfoldl(fun add_func_infos_fun_fold/2, {Labels, Mod}, Code),
  {ok, State#{code => NewCode, labels => NewLabels}}.

add_func_infos_fun_fold({FunName = {F, A}, Ops}, {Labels, Mod}) ->
  FuncInfo = {op, func_info, [Mod, F, A]},
  {NewOps, NewLabels} = add_func_infos_ops_fold(Ops, FuncInfo, Labels, []),
  {{FunName, NewOps}, {NewLabels, Mod}}.

add_func_infos_ops_fold([], _FuncInfo, Labels, Acc) ->
  {lists:reverse(Acc), Labels};
add_func_infos_ops_fold([{label, Label} = Op | Ops], FuncInfo, Labels, Acc) ->
  case maps:get(Label, Labels) of
    #{function := false} ->
      add_func_infos_ops_fold(Ops, FuncInfo, Labels, [Op | Acc]);
    #{function := true} ->
      NewAcc = case Acc of
                 [] ->
                   Acc;
                 [{op, Jmp, _} | _] when Jmp =:= return;
                                         Jmp =:= call_only;
                                         Jmp =:= call_last;
                                         Jmp =:= call_ext_only;
                                         Jmp =:= call_ext_last;
                                         Jmp =:= wait;
                                         Jmp =:= loop_rec_end;
                                         Jmp =:= select_valk;
                                         Jmp =:= jump->
                   Acc;
                 _Other ->
                   [{op, jump, [{label, Label}]} | Acc]
               end,
      FuncInfoLabel = make_ref(),
      NewLabels = Labels#{FuncInfoLabel => #{function => false}},
      RFuncInfoOps = [FuncInfo, {label, FuncInfoLabel}],
      add_func_infos_ops_fold(Ops, FuncInfo, NewLabels, [Op | RFuncInfoOps] ++ NewAcc)
  end;
add_func_infos_ops_fold([Op | Ops], FuncInfo, Labels, Acc) ->
  add_func_infos_ops_fold(Ops, FuncInfo, Labels, [Op | Acc]).

number_labels(State = #{code := Code}) ->
  Labels = lists:foldl(fun number_labels_fun_fold/2, #{"@0" => 0}, Code),
  {ok, State#{labels => Labels}}.

number_labels_fun_fold({_FunName, Ops}, Labels) ->
  lists:foldl(fun number_labels_ops_fold/2, Labels, Ops).

number_labels_ops_fold({label, Label}, Labels) when Label =/= "@@" ->
  Labels#{Label => maps:size(Labels)};
number_labels_ops_fold(_Op, Labels) ->
  Labels.

to_asm_terms(State = #{ code := Code
                      , labels := Labels
                      , exports := Exports
                      , module := Module
                      }) ->
  case to_asm_terms_fun_fold(Code, Labels, []) of
    {ok, NewCode} ->
      Header = [ {module, Module}
               , {exports, Exports}
               , {attributes, []}
               , {labels, maps:size(Labels)}
               ],
      {ok, State#{code => Header ++ NewCode}};
    {error, _} = Error ->
      Error
  end.

to_asm_terms_fun_fold([], _Labels, Acc) ->
  {ok, lists:reverse(Acc)};
to_asm_terms_fun_fold([{{F, A}, Ops} | Funs], Labels, Acc) ->
  [{label, FuncClauseLabel}, {op, func_info, _}, {label, Label} | _] = Ops,
  FunctionTerm = {function, F, A, maps:get(Label, Labels)},
  ExtendedLabels = Labels#{"@@" => maps:get(FuncClauseLabel, Labels)},
  case to_asm_terms_ops_fold(Ops, ExtendedLabels, [FunctionTerm | Acc]) of
    {ok, NewAcc} ->
      to_asm_terms_fun_fold(Funs, Labels, NewAcc);
    {error, _} = Error ->
      Error
  end.

to_asm_terms_ops_fold([], _Labels, Acc) ->
  {ok, Acc};
to_asm_terms_ops_fold([Op | Ops], Labels, Acc) ->
  case to_asm_term(Op, Labels) of
    {ok, Term} ->
      to_asm_terms_ops_fold(Ops, Labels, [Term | Acc]);
    {error, _} = Error ->
      Error
  end.

to_asm_term({label, Label}, Labels) ->
  {ok, {label, maps:get(Label, Labels)}};
to_asm_term({test, Op, Label, Args}, Labels) ->
  case encode_all([Label | Args], Labels, [integers_as_terms]) of
    {ok, [F | Terms]} ->
      {ok, {test, Op, F, Terms}};
    {error, _} = Error ->
      Error
  end;
to_asm_term({op, func_info, [M, F, A]}, _Labels) when is_atom(M),
                                                      is_atom(F),
                                                      is_integer(A) ->
  {ok, {func_info, {atom, M}, {atom, F}, A}};
to_asm_term({op, Op, []}, _Labels) ->
  {ok, Op};
to_asm_term({op, Op, Args}, Labels) when Op =:= call_ext;
                                         Op =:= call_ext_only;
                                         Op =:= call_ext_last ->
  case encode_all(Args, Labels, [funs_as_extfuncs]) of
    {ok, Terms = [{extfunc, _M, _F, A} | _]} ->
      {ok, list_to_tuple([Op, A | Terms])};
    {error, _} = Error ->
      Error
  end;
to_asm_term({op, BifOp, [Bif | Args]}, Labels) when BifOp =:= bif;
                                                    BifOp =:= gc_bif->
  case encode_all(Args, Labels, [lists_as_nonliterals]) of
    {ok, Ts} ->
      {ok, list_to_tuple([BifOp, Bif | Ts])};
    {error, _} = Error ->
      Error
  end;
to_asm_term({op, select_tuple_arity, Args = [_, _, _]}, Labels) ->
  case encode_all(Args, Labels, [lists_as_lists]) of
    {ok, [Reg, Label, List]} ->
      {ok, list_to_tuple([select_tuple_arity, Reg, Label, {list, List}])};
    {error, _} = Error ->
      Error
  end;
to_asm_term({op, move, Args}, Labels) ->
  case encode_all(Args, Labels, [integers_as_terms]) of
    {ok, Ts} ->
      {ok, list_to_tuple([move | Ts])};
    {error, _} = Error ->
      Error
  end;
to_asm_term({op, Op, Args}, Labels) ->
  case encode_all(Args, Labels) of
    {ok, Terms} ->
      {ok, list_to_tuple([Op | Terms])};
    {error, _} = Error ->
      Error
  end.

encode_all(Args, Labels) ->
  encode_all(Args, Labels, []).

encode_all(Args, Labels, Options) ->
  encode_all(Args, Labels, Options, []).

encode_all([], _Labels, _Options, Acc) ->
  {ok, lists:reverse(Acc)};
encode_all([Arg | Args], Labels, Options, Acc) ->
  case encode(Arg, Labels, Options) of
    {ok, Term} ->
      encode_all(Args, Labels, Options, [Term | Acc]);
    {error, _} = Error ->
      Error
  end.

encode({register, R}, _Labels, _Options) ->
  {ok, R};
encode({label, Label}, Labels, _Options) ->
  case maps:find(Label, Labels) of
    {ok, F} ->
      {ok, {f, F}};
    error ->
      make_error("label ~p is not defined", [Label])
  end;
encode(I, _Labels, Options) when is_integer(I) ->
  case proplists:get_bool(integers_as_terms, Options) of
    true ->  {ok, {integer, I}};
    false -> {ok, I}
  end;
encode(F, _Labels, _Options) when is_float(F) ->
  {ok, {float, F}};
encode(A, _Labels, _Options) when is_atom(A) ->
  {ok, {atom, A}};
encode(Fun, _Labels, Options) when is_function(Fun) ->
  case proplists:get_bool(funs_as_extfuncs, Options) of
    true ->
      {module, M} = erlang:fun_info(Fun, module),
      {name,   F} = erlang:fun_info(Fun, name),
      {arity,  A} = erlang:fun_info(Fun, arity),
      {ok, {extfunc, M, F, A}};
    false ->
      {literal, Fun}
  end;
encode(L, Labels, Options) when is_list(L) ->
  case proplists:get_bool(lists_as_nonliterals, Options) of
    true ->
      encode_all(L, Labels, [{integers_as_terms, true} | proplists:delete(lists_as_nonliterals, Options)]);
    false ->
      case lists:any(fun ({register, _}) -> true;
                         ({label, _}) -> true;
                         (_) -> false
                     end, L) of
        true ->
          encode_all(L, Labels, Options);
         false->
          {ok, {literal, encode_literal(L)}}
      end
  end;
encode(T, _Labels, _Options) ->
  {ok, {literal, encode_literal(T)}}.

encode_literal({tuple, T}) ->
  list_to_tuple(encode_literal(tuple_to_list(T)));
encode_literal(T) when is_list(T) ->
  lists:map(fun encode_literal/1, T);
encode_literal(T) when is_map(T) ->
  maps:from_list([{encode_literal(K), encode_literal(V)} || {K, V} <- maps:to_list(T)]);
encode_literal(T) when not is_tuple(T) ->
  T.

lay_out(State = #{code := Code}) ->
  {ok, State#{code => lists:map(fun lay_out_term/1, Code)}}.

lay_out_term(Term) ->
  Op = if is_atom(Term) -> Term;
          is_tuple(Term) -> element(1, Term)
       end,
  {EmptyLines, Ident} = lay_out_info(Op),
  io_lib:format("~*c~*c~p.~n", [EmptyLines, $\n, Ident, $\s, Term]).

lay_out_info(module)     -> {0, 0};
lay_out_info(exports)    -> {1, 0};
lay_out_info(attributes) -> {1, 0};
lay_out_info(labels)     -> {1, 0};
lay_out_info(function)   -> {2, 0};
lay_out_info(label)      -> {0, 2};
lay_out_info(_Other)     -> {0, 4}.

write_file(State = #{write_path := WritePath, module := Module, code := Code}) ->
  Name = filename:join(WritePath, Module) ++ ".S",
  case file:write_file(Name, Code) of
    ok -> {ok, State};
    {error, Reason} ->
      {error, {file, Reason}}
  end.

do_steps([], _State) ->
  ok;
do_steps([Fun | Funs], State) ->
  case Fun(State) of
    {ok, NewState} ->
      do_steps(Funs, NewState);
    {error, Error} ->
      format_error(maps:get(file, State), Error),
      halt(length(Funs) + 1)
  end.

make_error(Fmt, Args) ->
  {error, {?MODULE, {Fmt, Args}}}.

format_error({Fmt, Args}) ->
  io_lib:format(Fmt, Args).

format_error(File, {Module, Info}) ->
  io:format(standard_error,
            "~s: ~s~n",
            [File, Module:format_error(Info)]);
format_error(File, {Line, Module, Info}) ->
  io:format(standard_error,
            "~s:~p: ~s~n",
            [File, Line, Module:format_error(Info)]).
