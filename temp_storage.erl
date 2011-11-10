%%%Created by: Eva-Lisa Kedborn, Jing Liu
%%%Creation date: 2011-11-08
%%%
-module(temp_storage).
-export([create_ets/0,insert/3,retrieve_all_data/0]).

%%Create a new ets table to store the downloaded pieces.
create_ets()->
    ets:new(downloadedPieces,[named_table]).

insert(Index,Hash,Data)->
    ets:insert(downloadedPieces,{Index,{Hash,Data}}).

retrieve_all_data()->
    Length = ets:last(downloadedPieces),
    retrieve_all_data(1,Length).
retrieve_all_data(N,Length)when N=<Length ->
    {_Index,{_Hash,Data}} = ets:lookup(downloadedPieces,N),
    [Data|retrieve_all_data(N+1,Length)];
retrieve_all_data(_N,_Length) ->
    [].

    
    
	   
    


    
    