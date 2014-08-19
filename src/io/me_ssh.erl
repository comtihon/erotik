%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. Июль 2014 17:29
%%%-------------------------------------------------------------------
-module(me_ssh).
-author("tihon").

-define(SSH_CHANNEL_TM, 5000).

%% API
-export([connect/5, send/2]).

-spec connect(string(), integer(), string(), string(), integer()) -> {ok, pid()}.
connect(Host, Port, Login, Password, Timeout) ->
	ssh:connect(Host, Port, [{user, Login}, {password, Password}, {silently_accept_hosts, true}], Timeout).

-spec send(pid(), term()) -> ok | {error, timeout} | {error, closed}.
send(SSHRef, Command) ->
	{ok, ChId} = ssh_connection:session_channel(SSHRef, ?SSH_CHANNEL_TM),
	{ssh_connection:exec(SSHRef, ChId, Command, ?SSH_CHANNEL_TM), ChId}.
