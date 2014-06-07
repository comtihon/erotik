-module(erotik_app).
%% Example application
-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    me_routers_sup:start_link().

stop(_State) ->
    ok.
