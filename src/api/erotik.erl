%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. июн 2014 13:58
%%%-------------------------------------------------------------------
-module(erotik).
-author("tihon").

%% API
-export([api_connect/5, ssh_connect/5]).
-export([command/2]).

-spec api_connect(string(), string(), integer(), string(), string()) -> {ok, pid()}.
api_connect(Name, Host, Port, Login, Password) ->
	Config = [{host, Host}, {port, Port}, {login, Login}, {password, Password}],
	me_connector:start_link(Name, {api, Config}).

-spec ssh_connect(string(), string(), integer(), string(), string()) -> {ok, pid()}.
ssh_connect(Name, Host, Port, Login, Password) ->
	Config = [{host, Host}, {port, Port}, {login, Login}, {password, Password}],
	me_connector:start_link(Name, {ssh, Config}).

-spec command(Router :: atom() | pid(), Command :: list()) -> any().
command(Router, Command) ->
	Timeout = case application:get_env(me_connector_wait_time) of
		          {ok, Time} -> Time;
		          undefined -> infinity
	          end,
	gen_server:call(Router, {command, Command}, Timeout).