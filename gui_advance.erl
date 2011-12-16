%%%---------------------------------------------------------------------
%%% Created by: Jody Swartz
%%% Creation date: 2011-10-19
%%% Refactored date: 2011-11-02
%%%--------------------------------------------------------------------- 
%%% Description module gui_advance
%%%--------------------------------------------------------------------- 
%%% This module handles the advance features of the GUI.
%%%--------------------------------------------------------------------- 
%%% Exports 
%%%--------------------------------------------------------------------- 
%%% start()
%%%   Start() begins the create_advance_feature()
%%%--------------------------------------------------------------------- 

-module(gui_advance).
-include_lib("wx/include/wx.hrl").

-export([start/2]).
-compile(export_all).

start(MainPanel, OuterSizer) ->
    create_advance_feature(MainPanel, OuterSizer). 

%%--------------------------------------------------------------------
%% Function: create_advance_feature/2
%% Purpose:  which handles the
%%  declaration of  widgets 
%%
%% Args: MainPanel, OuterSizer
%% Returns: CreateText1104, CompleteText1105,  HashText1106 
%%--------------------------------------------------------------------

create_advance_feature(MainPanel, OuterSizer) ->  
    Notebook = wxNotebook:new(MainPanel, 1, [{style, ?wxBK_DEFAULT}]),
    Peers = wxBitmap:new("peers.xpm", [{type, ?wxBITMAP_TYPE_XPM}]),
    IL = wxImageList:new(16, 16),
    wxImageList:add(IL, Peers),
    wxNotebook:assignImageList(Notebook, IL),
    
   {CreateText1104, CompleteText1105,  HashText1106} = file_info_page(Notebook),
    {TorNameText1201, TpText1204, CpText1205} = torrent_info_page(Notebook),
   
      
    wxSizer:add(OuterSizer, Notebook, [{proportion, 0}, {flag, ?wxEXPAND}]),
    {TorNameText1201, CreateText1104, CompleteText1105, HashText1106, TpText1204, CpText1205}.

%%--------------------------------------------------------------------
%% Function: file_info_page/1
%% Purpose: This handles declaration variables, and setting of the sizing 
%% framework for the page for information on the file info.
%% Args: Notebook
%% Returns: CreateText1104, CompleteText1105,  HashText1106
%%--------------------------------------------------------------------

file_info_page(Notebook) ->
    Fpage = wxPanel:new(Notebook, []),
 
 
    CreateText1104 = wxStaticText:new(Fpage, 1104, "Created on: ", []),
    CompleteText1105 = wxStaticText:new(Fpage, 1105, "Completed on: ", []),
    HashText1106 = wxStaticText:new(Fpage, 1106, "Hash: ", []),
    FileBoxSizer = wxStaticBoxSizer:new(?wxVERTICAL, Fpage, 
					[{label, "File Information"}]),

    FileGridSizer = wxGridSizer:new(4, 2, 5, 5),
    wxSizer:add(FileBoxSizer, FileGridSizer, [{proportion, 0}, 
					      {flag, ?wxALIGN_LEFT}]),
    
    wxSizer:add(FileGridSizer, 30,10, []),
    wxSizer:add(FileGridSizer, 30, 10, []),
    wxSizer:add(FileGridSizer,  30, 10, []), 
    wxSizer:add(FileGridSizer, 30, 10, []),
    wxSizer:add(FileGridSizer, CreateText1104, [{proportion, 0}, 
						{flag, ?wxEXPAND}]),
  
    wxSizer:add(FileGridSizer, CompleteText1105, [{proportion, 0}, 
						    {flag, ?wxEXPAND}]), 
    wxSizer:add(FileGridSizer, HashText1106, [{proportion, 0}, 
						{flag, ?wxEXPAND}]),
    wxSizer:add(FileGridSizer, 30, 10, []), 
   
    wxPanel:setSizer(Fpage, FileBoxSizer),
    wxSizer:layout(FileBoxSizer),
    wxNotebook:addPage(Notebook, Fpage, "File Info", []),
    {CreateText1104, CompleteText1105,  HashText1106}.

%%--------------------------------------------------------------------
%% Function: torrent_info_page/1
%% Purpose: The handling the torrent info in regards to declaration, 
%%            framework of the structure of the torrent info page
%% Args: Notebook
%% Returns:  TorNameText1201, 
%%--------------------------------------------------------------------

torrent_info_page(Notebook) ->
  
    Tpage = wxPanel:new(Notebook, []),

    TorNameText1201 = wxStaticText:new(Tpage, 1201, "Torrent Name: ", []),
  
    TpText1204 = wxStaticText:new(Tpage, 1204, "Total Pieces:", []), 
    CpText1205 = wxStaticText:new(Tpage, 1205, 
				  "Current number of pieces: ", []),
    TorrentBoxSizer = wxStaticBoxSizer:new(?wxVERTICAL, Tpage, 
					      [{label, "Torrent Information"}]),
    TorrentGridSizer = wxGridSizer:new(3, 2, 5, 5),
    wxSizer:add(TorrentBoxSizer, TorrentGridSizer, 
		[{proportion, 0}, {flag, ?wxALIGN_LEFT}]),
    wxSizer:add(TorrentGridSizer, TorNameText1201, 
			    [{proportion, 0}, {flag, ?wxALIGN_LEFT}]),
    wxSizer:add(TorrentGridSizer, 30, 10, []),
    wxSizer:add(TorrentGridSizer, 30, 10, []),
    wxSizer:add(TorrentGridSizer, 30,10, [{proportion, 0}, 
						  {flag, ?wxALIGN_RIGHT}]), 
    wxSizer:add(TorrentGridSizer, TpText1204, [{proportion, 0}, 
					       {flag, ?wxALIGN_LEFT}]),
    wxSizer:add(TorrentGridSizer, CpText1205, [{proportion, 0}, 
					       {flag, ?wxALIGN_RIGHT}]),

    wxPanel:setSizer(Tpage, TorrentBoxSizer ),
    wxNotebook:addPage(Notebook, Tpage, "Torrent Info", []),
   {TorNameText1201, TpText1204, CpText1205}.
    

    
