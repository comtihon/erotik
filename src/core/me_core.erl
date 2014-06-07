%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 06. июн 2014 0:59
%%%-------------------------------------------------------------------
-module(me_core).
-author("tihon").

%% API
-export([decode_len/2, encode_len/1, hex_to_binary/1, binary_to_hex/1]).

%TODO change to stream decoding
decode_len(Len, Socket) when (Len band 16#E0) == 16#E0 -> % 4 bytes len
	First = Len band 16#1F,
	{ok, Rest} = gen_tcp:recv(Socket, 3),
	<<First, Rest>>;
decode_len(Len, Socket) when (Len band 16#C0) == 16#C0 -> % 3 bytes len
	First = Len band 16#3f,
	{ok, Rest} = gen_tcp:recv(Socket, 2),
	<<First, Rest>>;
decode_len(Len, Socket) when (Len band 16#80) == 16#80 -> % 2 bytes len
	First = Len band 16#7f,
	{ok, Rest} = gen_tcp:recv(Socket, 1),
	<<First, Rest>>;
decode_len(Len, _) -> Len.  % 1 bytes len

encode_len(Len) when Len < 16#80 -> binary:encode_unsigned(Len); % 1 bytes len
encode_len(Len) when Len < 16#4000 -> encode(Len, 16#80); % 2 bytes len
encode_len(Len) when Len < 16#200000 -> encode(Len, 16#c0); % 3 bytes len
encode_len(Len) when Len < 16#10000000 -> encode(Len, 16#e0). % 4 bytes len

binary_to_hex(BinList) ->
	lists:foldr(
		fun(Dec, Acc) ->
			[integer_to_list(Dec, 16) | Acc]
		end, [], BinList).
hex_to_binary(HexList) ->
	Splitted = part(HexList),
	lists:foldr(
		fun(Hex, Acc) ->
			[list_to_integer(Hex, 16) | Acc]
		end, [], Splitted).

%% @private
encode(Len, Bor) ->
	Binary = binary:encode_unsigned(Len), %TODO endiannes
	[First | Tail] = binary:bin_to_list(Binary),  %TODO better solution?
	list_to_binary([First bor Bor | Tail]).

%% @private
part(List) when length(List) == 32 ->
	part(List, []).
part([], Acc) ->
	lists:reverse(Acc);
part([H1, H2 | T], Acc) ->
	part(T, [[H1, H2] | Acc]).