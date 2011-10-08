%%%Created by: Fredrik Gustafsson.
%%%Creation date: 111007.
%%%Info: Connects to specified tracker and retrieves info about the file specified.
-module(connect_to_tracker).
-export([get_info/1]).
%%This function takes one argument, the url to the tracker.
get_info(Url) ->
    inets:start(),
    {ok, {_,_,Result}} = httpc:request(Url),
    case [hd(Result)] of
	"d" ->
	    bencode:decode(list_to_binary(Result));
	_  ->
	    {error, error_from_tracker}
    end.
