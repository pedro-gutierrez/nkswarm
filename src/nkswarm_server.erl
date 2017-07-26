-module(nkswarm_server).
-behavior(gen_server).
-export([start_link/0]).
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).
-record(data, {status, replied, contacted, nodes, last_checked}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, #data{status=initializing}, 0}.

handle_info(timeout, Data) ->
    Data2 = contact(Data),
    {noreply, Data2}.

handle_call(status, _, Data) ->
    Data2 = contact(Data),
    {reply, to_map(Data2), Data2}.

handle_cast(_, Data) ->
  {noreply, Data}.

terminate(_Reason, _Data) ->
  ok.

code_change(_OldVsn, Data, _Extra) ->
  {ok, Data}.

contact(Data) ->
    case nkswarm_config:contact() of
        [] -> 
            Data#data{status=disabled, 
                      replied=[], 
                      contacted=[], 
                      nodes=nodes(),
                      last_checked=millis()};
        Nodes -> 
            Timeout = nkswarm_config:timeout(),
            contact([N || N <- Nodes, N =/= node()], Timeout, Data)
    end.

contact(Nodes, Timeout, Data) ->
    Answering = [N || N <- Nodes, net_adm:ping(N) =:= pong],
    case Answering of
        [] -> 
            Data#data{status=no_nodes, 
                      replied=Answering, 
                      contacted=Nodes, 
                      nodes=nodes(),
                      last_checked=millis()};
        _ -> 
            wait_for_nodes(Answering, Nodes, Timeout, Data)
    end.

wait_for_nodes(Answering, Nodes, WaitTime, Data) ->
    Slices = 10,
    SliceTime = round(WaitTime/Slices),
    wait_for_nodes(Answering, Nodes, SliceTime, Slices, Data).

wait_for_nodes(Answering, Nodes, _SliceTime, 0, Data) ->
    Data#data{status=first_node, 
              replied=Answering, 
              contacted=Nodes,
              nodes=nodes(),
              last_checked=millis()};

wait_for_nodes(Answering, Nodes, SliceTime, Iterations, Data) -> 
    case length(nodes()) > length(Answering) of
        true -> 
            Data#data{status=ok, 
                      replied=Answering, 
                      contacted=Nodes, 
                      nodes=nodes(),
                      last_checked=millis()};
        false ->
            timer:sleep(SliceTime),
            wait_for_nodes(Answering, Nodes, SliceTime, Iterations - 1, Data)
    end.


millis() ->
    erlang:system_time(millisecond).

to_map(#data{status=Status, replied=Replied, contacted=Contacted, nodes=Nodes, last_checked=Checked}) ->
    #{ status => Status, 
       replied => Replied,
       contacted => Contacted, 
       nodes => Nodes, 
       last_checked => Checked }.