%% created by Jing Liu
%% URLencoding algorithm created by Fredrik Gustafsson
%% creation date: 18 Oct 2011


-module sha.
-export([sha1hash/1,urlencode/1,shaurl/1, chunk_it_up/1, sha1raw/1]).

%%--------------------------------------------------------------------
%% Function:sha1hash/1
%% Purpose: sha1hash data and return a list of intgers on the base of 16
%% Args: Data to be hashed
%% Returns:  a list of intgers on the base of 16
%%--------------------------------------------------------------------

sha1hash(Data)->
    crypto:start(),
    L = [ hd(integer_to_list(N, 16)) || << N:4 >> <= crypto:sha(Data) ],
    L.
%%--------------------------------------------------------------------
%% Function: sha1raw/1
%% Purpose: sha1hash the data and return a list of integers same with 
%%          the binary value
%% Args: Data to be hashed
%% Returns: a list of integers same with the binary value
%%--------------------------------------------------------------------

sha1raw(Data) ->
    crypto:start(),
    binary_to_list(crypto:sha(Data)).

%% library function for url encoding
urlencode(Sha)->
    edoc_lib:escape_uri(Sha).

%%--------------------------------------------------------------------
%% Function: shaurl/1
%% Purpose: sha1 and url encoding combined
%% Args: Data to be hashed
%% Returns: hashed and url encoded data
%%--------------------------------------------------------------------

shaurl(Data)->
    L = sha1hash(Data),
    chunk_it_up(L).

%% inner function for url encoding
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

%% inner function for url encoding
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
