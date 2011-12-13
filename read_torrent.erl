%%%Created by: Fredrik Gustafsson.
%%%Creation date: 111005.
%%%Info: Reads in and parses the bencoded torrent and gets the information from the tracker.
-module(read_torrent).
-export([start/0]).

%%This function takes two arguments, Name and File, where Name is the name of the torrent and File is the path to it
start() ->
    receive
	{read, From, File} ->
	    case  file:read_file(File) of
		{ok, Text} ->
		    case bencode:decode(Text) of
			{{dict, Dec}, _Remainder} ->
			    From !  {reply, {dict, Dec}}
		    end;
		_  ->
		    {error, no_file}
	    end
    end.
