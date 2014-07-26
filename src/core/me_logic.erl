%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 02. июн 2014 22:48
%%%-------------------------------------------------------------------
-module(me_logic).
-author("tihon").

%% API
-export([do_login/3, send_command/2]).

send_command(SSHRef, Command) when is_pid(SSHRef) ->  %ssh
	{success, ChannelId} = me_ssh:send(SSHRef, Command),
	ChannelId;
send_command(Socket, Command) when not is_list(Command) -> send_command(Socket, [Command]); %api
send_command(Socket, Command) ->
	me_api:write_sentence(Socket, Command),
	me_api:read_block(Socket).

do_login(Socket, Login, Password) ->
	me_api:write_sentence(Socket, ["/login"]),
	Salt = get_salt(me_api:read_sentence(Socket)),
	Hash = count_hash(Password, Salt),
	LoginRequest = form_login_sentence(Login, Hash),
	me_api:write_sentence(Socket, LoginRequest),
	{done, ["!done"]} = me_api:read_sentence(Socket),
	ok.

%% @private
get_salt({done, LoginGreeting}) ->
	[_, Code] = LoginGreeting,
	Code -- "=ret=".

%% @private
count_hash(Password, Salt) ->
	BinMd5 = me_core:hex_to_binary(Salt),
	Concat = lists:append([0 | Password], BinMd5),
	BinHex = crypto:hash(md5, Concat),
	me_core:binary_to_hex(binary_to_list(BinHex)).

%% @private
form_login_sentence(Login, Hash) ->
	Name = lists:append("=name=", Login),
	Code = lists:append("=response=00", lists:append(Hash)),
	["/login", Name, Code].