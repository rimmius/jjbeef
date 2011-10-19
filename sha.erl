%%% @author  <Jing@LENOVO-PC>
%%% @copyright (C) 2011, 
%%% @doc
%%%
%%% @end
%%% Created : 18 Oct 2011 by  <lenovo@LENOVO-PC>


-module sha.
-export([sha1hash/1,urlencode/1,shaurl/1]).

%%only sha1 hash
sha1hash(Data)->
    crypto:start(),
    L = [ hd(integer_to_list(N, 16)) || << N:4 >> <= crypto:sha(Data) ],
    L.

%%only urlencoding
urlencode(Sha)->
    edoc_lib:escape_uri(Sha).

%%sha1hash + urlencoding
shaurl(Data)->
    L = sha1hash(Data),
    urlencode(L).
