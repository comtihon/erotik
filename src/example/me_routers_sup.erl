-module(me_routers_sup).
%% Example top-level supervisor, which creates one worker per router from apps conf
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(CHILD(I, Name, Type, Params), {Name, {I, start_link, Params}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
	{ok, Routers} = application:get_env(routers),
	Children = lists:foldl(
		fun(Router, Acc) ->
			Host = proplists:get_value(host, Router),
			Port = proplists:get_value(port, Router),
			Name = lists:concat([Host, ":", Port]),
			[?CHILD(me_connector, Name, worker, [Name, {api, Router}]) | Acc]
		end, [], Routers),
	{ok, {{one_for_one, 5, 10}, Children}}.