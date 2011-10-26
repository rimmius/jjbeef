%%% @author  <Jing@LENOVO-PC>
%%% @copyright (C) 2011, 
%%% @doc
%%%
%%% @end
%%% Created : 18 Oct 2011 by  <lenovo@LENOVO-PC>


-module sha.
-export([sha1hash/1,urlencode/1,shaurl/1, chunk_it_up/1, sha1raw/1]).

%%only sha1 hash
sha1hash(Data)->
    crypto:start(),
    L = [ hd(integer_to_list(N, 16)) || << N:4 >> <= crypto:sha(Data) ],
    L.
sha1raw(Data) ->
    crypto:start(),
    binary_to_list(crypto:sha(Data)).
%%only urlencoding
urlencode(Sha)->
    edoc_lib:escape_uri(Sha).

%%sha1hash + urlencoding
shaurl(Data)->
    L = sha1hash(Data),
    chunk_it_up(L).
chunk_it_up([]) ->
    [];
chunk_it_up([H|T]) ->
    {List1, List2} = lists:split(2, [H|T]),
    case hd(List1) of
	48 ->
	    ["%" ++ List1|chunk_it_up(List2)];
	_ ->
	    [check_digits(http_util:hexlist_to_integer(List1))|chunk_it_up(List2)]
    end.

check_digits(N) when (N >= 65) and (N =< 90) ->
    [N];
check_digits(N) when (N >= 97) and (N =< 122) ->
    [N];
check_digits(N) when (N >= 48) and (N =< 57) ->
    [N];
check_digits(N) when N =:= 45, N =:= 95, N =:= 46, N =:= 126 ->
    [N];
check_digits(N) ->
    "%" ++ http_util:integer_to_hexlist(N).
