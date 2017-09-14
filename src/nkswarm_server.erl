-module(nkswarm_server).
-behavior(gen_statem).
-export([start_link/0]).
-export([init/1, callback_mode/0, terminate/3]).
-export([active/3, passive/3]).
-record(data, {beacon, cluster}).
-define(APP, nkswarm).

callback_mode() ->
    state_functions.

start_link() ->
    gen_statem:start_link({local, ?MODULE}, ?MODULE, [], []).

config() -> 
    { application:get_env(?APP, port, 9999),
      application:get_env(?APP, cluster, nkswarm),
      application:get_env(?APP, timeout, 1000),
      application:get_env(?APP, interval, 300)}.

init([]) ->
    {Port, ClusterName, Timeout, Interval} = config(),    
    {ok, B} = rbeacon:new(Port, [active, noecho]),
    rbeacon:set_interval(B, Interval),
    Ann = encode({ClusterName, node()}),
    rbeacon:subscribe(B, <<>>), 
    rbeacon:publish(B, Ann),
    {ok, active, #data{beacon=B, cluster=ClusterName}, Timeout}.

active(timeout, _, #data{beacon=B}=Data) ->
    log({active, timeout, nodes(), going_passive}),
    rbeacon:silence(B),
    {next_state, passive, Data};

active(info, {rbeacon, _, Msg, _}, #data{beacon=B, cluster=C}=Data) ->
    case decode(Msg) of
        {C, Peer} ->
            case net_adm:ping(Peer) of
                pong -> 
                    log({active, discovered, Peer, going_passive}),
                    rbeacon:silence(B),
                    {next_state, passive, Data};
                _ ->
                    log({active, could_not_ping, Peer, staying_active}),
                    {next_state, active, Data}
            end;
        _ -> 
            {next_state, active, Data}
    end.

passive(info, {rbeacon, _, Msg, _}, #data{beacon=B, cluster=C}=Data) ->
    case decode(Msg) of
        {C, Peer} ->
            case net_adm:ping(Peer) of
                pong -> 
                    log({passive, discovered, Peer, staying_passive}),
                    rbeacon:silence(B),
                    {next_state, passive, Data};
                _ ->
                    log({passive, could_not_ping, Peer, staying_passive}),
                    {next_state, active, Data}
            end;
        _ -> 
            {next_state, active, Data}
    end.

terminate(Reason, _, #data{beacon=B}) ->
    log({terminated, Reason}),
    rbeacon:close(B),
    ok.

encode(Term) ->
    erlang:term_to_binary(Term).

decode(Bin) ->
    erlang:binary_to_term(Bin).

log(Term) ->
    io:format("[nkswarm] ~p~n", [Term]).
