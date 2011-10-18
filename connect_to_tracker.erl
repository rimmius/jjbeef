%%%Created by: Fredrik Gustafsson.
%%%Creation date: 111007.
%%%Info: Connects to specified tracker and retrieves info about the file specified.
-module(connect_to_tracker).
-export([start/0,get_info/1]).
%%This function takes one argument, the url to the tracker.
start() ->
    receive
	{From, Url} ->
	    From ! get_info(Url)
    end.
get_info(Url) ->
    inets:start(),
    Result = httpc:request(Url),
    io:format("~w~n", [Result]),
    Result.
