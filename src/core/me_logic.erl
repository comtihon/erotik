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
-export([do_login/3]).

do_login(Socket, Login, Password) ->
	me_transport:write_sentence(Socket, ["/login"]),
	Salt = get_salt(me_transport:read_sentence(Socket)),
	Hash = count_hash(Password, Salt),
	LoginRequest = form_login_sentence(Login, Hash),
	me_transport:write_sentence(Socket, LoginRequest),
	{done, ["!done"]} = me_transport:read_sentence(Socket),
	ok.

get_salt({done, LoginGreeting}) ->
	[_, Code] = LoginGreeting,
	Code -- "=ret=".

count_hash(Password, Salt) ->
	BinMd5 = me_core:hex_to_binary(Salt),
	Concat = lists:append([0 | Password], BinMd5),
	BinHex = crypto:hash(md5, Concat),
	me_core:binary_to_hex(binary_to_list(BinHex)).

form_login_sentence(Login, Hash) ->
	Name = lists:append("=name=", Login),
	Code = lists:append("=response=00", lists:append(Hash)),
	["/login", Name, Code].