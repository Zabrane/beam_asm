-module(reftrick_test).

-behaviour(perftest).
-export([ args/0
        , setup/1
        , validate/3
        , cleanup/1
        ]).

-export([ server/0
        ]).

args() ->
  #{ msg_queue => lists:reverse([0, 10, 100, 1000, 2500, 5000, 7500, 10000])
   }.

setup(#{msg_queue := MsgQueue}) ->
  Ps = [ spawn_link(?MODULE, server, [])
       , spawn_link(?MODULE, server, [])
       ],
  send_msgs(MsgQueue),
  {ok, Ps, Ps}.

validate(normal, {ok, P}, Ps) ->
  lists:member(P, Ps);
validate(_, _, _) ->
  false.

cleanup(Ps) ->
  [begin
     unlink(P),
     exit(P, kill)
   end
   || P <- Ps],
  ok.

server() ->
  receive
    {request, Tag, From} ->
      From ! {response, Tag, self()}
  end,
  server().

send_msgs(0) ->
  ok;
send_msgs(N) when is_integer(N), N > 0 ->
  self() ! message,
  send_msgs(N - 1).
