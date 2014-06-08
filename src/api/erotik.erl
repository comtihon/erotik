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
-export([get_users/1]).


-spec get_users(Router :: atom() | pid()) -> any().
get_users(Router) ->
	gen_server:call(Router, {command, ["/interface/getall"]}).