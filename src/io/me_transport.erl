%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 29. май 2014 22:35
%%%-------------------------------------------------------------------
-module(me_transport).
-author("tihon").

-include("me_transport.hrl").

%% API
-export([read_sentence/1, write_sentence/2]).

write_sentence(Socket, Sentence) ->
	lists:foreach(fun(Word) -> write_word(Socket, Word) end, Sentence),
	write_word(Socket, ?EOF).

read_sentence(Socket) -> read_sentence(Socket, [], false).
read_sentence(Socket, Acc, Spec) ->
	Word = read_word(Socket),
	case Word of
		[] -> {Spec, lists:reverse(Acc)};
		"!done" -> read_sentence(Socket, [Word | Acc], done);
		"!trap" -> read_sentence(Socket, [Word | Acc], trap);
		"!fatal" -> read_sentence(Socket, [Word | Acc], fatal);
		Data -> read_sentence(Socket, [Data | Acc], Spec)
	end.

%% @private
-spec write_word(Socket :: port(), Word :: string()) -> ok.
write_word(Socket, Word) ->
	Len = me_core:encode_len(length(Word)), % encode length
	ok = gen_tcp:send(Socket, Len),  % send length
	ok = gen_tcp:send(Socket, Word). % send data then

%% @private
read_word(Socket) ->
	{ok, FirstLen} = gen_tcp:recv(Socket, 1),
	case binary:decode_unsigned(me_core:decode_len(list_to_binary(FirstLen), Socket)) of
		0 -> [];
		Len ->
			{ok, Word} = gen_tcp:recv(Socket, Len),
			Word
	end.