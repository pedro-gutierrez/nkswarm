-module(nkswarm_config).
-export([timeout/0, contact/0]).
-define(APP, nkswarm).

timeout() -> 
    get_env(?APP, timeout, 10000).

contact() -> 
    get_env(?APP, contact, []).

get_env(AppName, Key, Default) ->
    case application:get_env(AppName, Key) of
        undefined   -> Default;
        {ok, Value} -> Value
    end.
