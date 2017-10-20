-module(nkswarm_server).
-export([start_link/0]).
-export([init/1, callback_mode/0, terminate/3]).
-export([stopped/3, active/3, passive/3]).
-export([status/0, start/2]).
-record(data, {beacon, cluster, srvid}).
-include("nkswarm.hrl").

callback_mode() ->
    state_functions.

start_link() ->
    gen_statem:start_link({local, ?MODULE}, ?MODULE, [], []).

start(SrvId, Config) ->
    gen_statem:call(?MODULE, {start, SrvId, Config}).

status() ->
    gen_statem:call(?MODULE, status).

init([]) ->
    log({stopped, nkswarm_server}),
    {ok, stopped, #data{}}.

stopped({call, From}, {start, SrvId, Config}, _) ->
    #{ discovery_port := Port, 
       discovery_name := ClusterName, 
       beacon_timeout := Timeout, 
       beacon_interval := Interval } = Config,    
    log(Config),
    {ok, B} = rbeacon:new(Port, [active, noecho, {mode, {unicast, ClusterName}}]),
    rbeacon:set_interval(B, Interval),
    Ann = encode({ClusterName, node()}),
    rbeacon:subscribe(B, <<>>), 
    rbeacon:publish(B, Ann),
    nkstats:register_metric(SrvId, gauge, ?NETCOMP_CLUSTER_SIZE, "Netcomposer Cluster Size"),
    publish_status(SrvId),
    {next_state, active, #data{beacon=B, cluster=ClusterName, srvid=SrvId}, 
        [{reply, From, ok},
         {{timeout,go_passive},Timeout,active}] }.

active({timeout, go_passive}, _, #data{beacon=B}=Data) ->
    log({active, timeout, nodes(), going_passive}),
    rbeacon:silence(B),
    {next_state, passive, Data};

active(info, {rbeacon, _, Msg, _}, #data{beacon=B, cluster=C, srvid=SrvId}=Data) ->
    case decode(Msg) of
        {C, Peer} ->
            case net_adm:ping(Peer) of
                pong -> 
                    log({active, discovered, Peer, going_passive}),
                    rbeacon:silence(B),
                    erlang:monitor_node(Peer, true),
                    publish_status(SrvId),
                    {next_state, passive, Data};
                _ ->
                    log({active, could_not_ping, Peer, staying_active}),
                    {next_state, active, Data}
            end;
        _ -> 
            {next_state, active, Data}
    end;

active({call, From}, status, #data{cluster=C}=Data) ->
    {keep_state, Data, {reply, From, status(C, active)}};

active(info, {nodedown, Node}, #data{srvid=SrvId}=Data) ->
    log({nodedown, Node}),
    publish_status(SrvId),
    {next_state, passive, Data}.


passive({timeout, go_passive}, _, #data{beacon=B}=Data) ->
    log({passive, timeout, nodes(), staying_passive}),
    rbeacon:silence(B),
    {next_state, passive, Data};

passive(info, {rbeacon, _, Msg, _}, #data{beacon=B, cluster=C, srvid=SrvId}=Data) ->
    case decode(Msg) of
        {C, Peer} ->
            case net_adm:ping(Peer) of
                pong -> 
                    log({passive, discovered, Peer, staying_passive}),
                    rbeacon:silence(B),
                    erlang:monitor_node(Peer, true),
                    publish_status(SrvId),
                    {next_state, passive, Data};
                _ ->
                    log({passive, could_not_ping, Peer, staying_passive}),
                    {next_state, active, Data}
            end;
        _ -> 
            {next_state, active, Data}
    end;

passive(info, {nodedown, Node}, #data{srvid=SrvId}=Data) ->
    log({nodedown, Node}),
    publish_status(SrvId),
    {next_state, passive, Data};

passive({call, From}, status, #data{cluster=C}=Data) ->
    {keep_state, Data, {reply, From, status(C, passive)}}.

terminate(Reason, _, #data{beacon=B}) ->
    log({terminated, Reason}),
    rbeacon:close(B),
    ok.

encode(Term) ->
    erlang:term_to_binary(Term).

decode(Bin) ->
    erlang:binary_to_term(Bin).

log(Term) ->
    ?INFO("~p", [Term]).

publish_status(SrvId) ->
    nkstats:record_value(SrvId, gauge, ?NETCOMP_CLUSTER_SIZE, length(nodes())+1).

status(Cluster, Status) ->
    #{ status => Status,
       cluster => Cluster,
       nodes => [node() | nodes()]}.
