%%%---------------------------------------------------------------------
%%% Copied and the modified from: 
%%% https://github.com/logaan/erlang-bittorrent/blob
%%% /fdfe4b87d596ad50d9368534fd6c1aa0ffebc40a/bencode.erl
%%% Creation date: 2011-10-07
%%%--------------------------------------------------------------------- 
%%% Description module bencode
%%%--------------------------------------------------------------------- 
%%% This module decodes and encodes information read from the *.torrent
%%% file that is bencoded. 
%%%--------------------------------------------------------------------- 
%%% Exports 
%%%--------------------------------------------------------------------- 
%%% decode(Binary)
%%%   decodes the binary information returned from reading the *.torrent
%%%   file
%%%   returns a tagged tuple {dict, Dict} with Dict being the dictonary
%%%   in the *.torrent file.
%%%--------------------------------------------------------------------- 
%%% encode(Element)
%%%   encodes to bencode as binary
%%%   returnes bencoded information as binaries
%%%--------------------------------------------------------------------- 

-module(bencode).
-export([decode/1, encode/1]).
-define(DICT, dict).

decode(<<$l, Tail/binary>>) ->
    decode_list(Tail, []);
decode(<<$d, Tail/binary>>) ->
    decode_dict(Tail, ?DICT:new());
decode(<<$i, Tail/binary>>) ->
    decode_integer(Tail, []);
decode(Data) ->
    decode_string(Data, []).

decode_integer(<<$e, T/binary>>, New_list) ->
    {list_to_integer(lists:reverse(New_list)), T};
decode_integer(<<H, T/binary>>, New_list) ->
    decode_integer(T, [H|New_list]).

decode_string(<<$:, T/binary>>, New_list) ->
    Integer = list_to_integer(lists:reverse(New_list)),
    <<String:Integer/binary, Rest/binary>> = T,
    {String, Rest};
decode_string(<<H, T/binary>>, New_list) ->
    decode_string(T, [H|New_list]).

decode_list(<<$e, T/binary>>, New_list) ->
    {{list, lists:reverse(New_list)}, T};
decode_list(Data, New_list) ->
    {Res, T} = decode(Data),
    decode_list(T, [Res|New_list]).

decode_dict(<<$e, T/binary>>, New_list) ->
    {{dict, New_list}, T};
decode_dict(Data, New_list) ->
    {Key, T1} = decode(Data),
    {Val, T2} = decode(T1),
    decode_dict(T2, ?DICT:store(Key, Val, New_list)).

encode(Integer) when is_integer(Integer) ->
    Bin = list_to_binary(integer_to_list(Integer)),
    [$i, Bin, $e];
encode(String) when is_list(String) ->
    encode(list_to_binary(String));
encode(String) when is_binary(String) ->
    Bin = list_to_binary(integer_to_list(size(String))),
    [Bin, $:, String];
encode({list, List}) when is_list(List) ->
    [$l, [encode(Elem) || Elem <- List], $e];
encode({dict, Dict}) ->
    Things = lists:map(
	     fun({Key, Val}) when is_list(Key) or is_binary(Key) ->
		     [encode(Key), encode(Val)]
	     end, lists:keysort(1, ?DICT:to_list(Dict))),
    [$d, Things, $e].
