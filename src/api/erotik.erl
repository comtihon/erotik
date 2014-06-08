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
-export([get_users/1, command/2, get_firewall/1]).


-spec get_users(Router :: atom() | pid()) -> any().
get_users(Router) ->
	command(Router, ["/interface/wireless/registration-table/print"]).

-spec get_users(Router :: atom() | pid()) -> any().
get_firewall(Router) ->
	command(Router, ["/ip/firewall/filter/print"]).

-spec command(Router :: atom() | pid(), Command :: list()) -> any().
command(Router, Command) ->
	Timeout = case application:get_env(me_connector_wait_time) of
		          {ok, Time} -> Time;
		          undefined -> infinity
	          end,
	gen_server:call(Router, {command, Command}, Timeout).