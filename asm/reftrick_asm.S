{module, reftrick_asm}.

{exports, [{module_info,0},{module_info,1},{run,2}]}.

{attributes, []}.

{labels, 14}.


{function, run, 2, 2}.
  {label,1}.
    {func_info,{atom,reftrick_asm},{atom,run},2}.
  {label,2}.
    {allocate_zero,4,2}.
    {move,{x,1},{y,2}}.
    {move,{x,0},{x,1}}.
    {move,{x,1},{y,3}}.
    {move,{atom,process},{x,0}}.
    {recv_mark,{f,3}}.
    {call_ext,2,{extfunc,erlang,monitor,2}}.
    {move,{x,0},{y,1}}.
    {move,{y,2},{x,1}}.
    {move,{atom,process},{x,0}}.
    {call_ext,2,{extfunc,erlang,monitor,2}}.
    {test_heap,4,1}.
    {bif,self,{f,0},[],{x,1}}.
    {move,{x,0},{y,0}}.
    {put_tuple,3,{x,2}}.
    {put,{atom,request}}.
    {put,{y,1}}.
    {put,{x,1}}.
    {move,{x,2},{x,1}}.
    {move,{y,3},{x,0}}.
    send.
    {test_heap,4,0}.
    {bif,self,{f,0},[],{x,0}}.
    {put_tuple,3,{x,1}}.
    {put,{atom,request}}.
    {put,{y,1}}.
    {put,{x,0}}.
    {move,{y,2},{x,0}}.
    send.
    {recv_set,{f,3}}.
  {label,3}.
    {loop_rec,{f,9},{x,0}}.
    {test,is_tuple,{f,8},[{x,0}]}.
    {select_tuple_arity,{x,0},{f,8},{list,[3,{f,4},5,{f,5}]}}.
  {label,4}.
    {get_tuple_element,{x,0},0,{x,1}}.
    {test,is_eq_exact,{f,8},[{x,1},{atom,response}]}.
    {get_tuple_element,{x,0},1,{x,2}}.
    {test,is_eq_exact,{f,8},[{x,2},{y,1}]}.
    {test_heap,3,1}.
    {get_tuple_element,{x,0},2,{x,3}}.
    remove_message.
    {put_tuple,2,{x,0}}.
    {put,{atom,ok}}.
    {put,{x,3}}.
    {deallocate,4}.
    return.
  {label,5}.
    {get_tuple_element,{x,0},0,{x,1}}.
    {test,is_eq_exact,{f,8},[{x,1},{atom,'DOWN'}]}.
    {get_tuple_element,{x,0},1,{x,2}}.
    {get_tuple_element,{x,0},2,{x,3}}.
    {test,is_eq_exact,{f,8},[{x,3},{atom,process}]}.
    {get_tuple_element,{x,0},3,{x,4}}.
    {get_tuple_element,{x,0},4,{x,5}}.
    {test,is_eq_exact,{f,6},[{x,2},{y,1}]}.
    {test,is_ne_exact,{f,7},[{x,4},{y,3}]}.
  {label,6}.
    {test,is_eq_exact,{f,8},[{x,2},{y,0}]}.
    {test,is_eq_exact,{f,8},[{x,4},{y,2}]}.
  {label,7}.
    {test_heap,3,6}.
    remove_message.
    {put_tuple,2,{x,0}}.
    {put,{atom,error}}.
    {put,{x,5}}.
    {deallocate,4}.
    return.
  {label,8}.
    {loop_rec_end,{f,3}}.
  {label,9}.
    {wait,{f,3}}.


{function, module_info, 0, 11}.
  {label,10}.
    {func_info,{atom,reftrick_asm},{atom,module_info},0}.
  {label,11}.
    {move,{atom,reftrick_asm},{x,0}}.
    {call_ext_only,1,{extfunc,erlang,get_module_info,1}}.


{function, module_info, 1, 13}.
  {label,12}.
    {func_info,{atom,reftrick_asm},{atom,module_info},1}.
  {label,13}.
    {move,{x,0},{x,1}}.
    {move,{atom,reftrick_asm},{x,0}}.
    {call_ext_only,2,{extfunc,erlang,get_module_info,2}}.
