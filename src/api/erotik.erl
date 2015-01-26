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
-export([api_connect/5, ssh_connect/5, api_connect/6, ssh_connect/6]).
-export([command/2]).

-spec api_connect(string(), string(), integer(), string(), string()) -> {ok, pid()}.
api_connect(Name, Host, Port, Login, Password) ->
	Config = [{host, Host}, {port, Port}, {login, Login}, {password, Password}],
	{ok, Pid} = me_connector:start_link(Name, []),
	ok = gen_server:call(Pid, {connect, {api, Config}}, 10 * 1000),
	{ok, Pid}.

%% same as api_connect, but with timeout in milliseconds
-spec api_connect(string(), string(), integer(), string(), string(), integer()) -> {ok, pid()}.
api_connect(Name, Host, Port, Login, Password, Timeout) ->
	Config = [{host, Host}, {port, Port}, {login, Login}, {password, Password}, {timeout, Timeout}],
	{ok, Pid} = me_connector:start_link(Name, []),
	ok = gen_server:call(Pid, {connect, {api, Config}}, 10 * 1000),
	{ok, Pid}.

-spec ssh_connect(string(), string(), integer(), string(), string()) -> {ok, pid()}.
ssh_connect(Name, Host, Port, Login, Password) ->
	Config = [{host, Host}, {port, Port}, {login, Login}, {password, Password}],
	me_connector:start_link(Name, {ssh, Config}).

%% same as ssh_connect, but with timeout in milliseconds
-spec ssh_connect(string(), string(), integer(), string(), string(), integer()) -> {ok, pid()}.
ssh_connect(Name, Host, Port, Login, Password, Timeout) ->
	Config = [{host, Host}, {port, Port}, {login, Login}, {password, Password}, {timeout, Timeout}],
	me_connector:start_link(Name, {ssh, Config}).

-spec command(Router :: atom() | pid(), Command :: list()) -> any().
command(Router, Command) ->
	Timeout = case application:get_env(me_connector_wait_time) of
		          {ok, Time} -> Time;
		          undefined -> infinity
	          end,
	gen_server:call(Router, {command, Command}, Timeout).