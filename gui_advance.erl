-module(gui_advance).
-include_lib("wx/include/wx.hrl").

-export([start/2]).
-compile(export_all).

start(MainPanel, OuterSizer) ->
    create_advance_feature(MainPanel, OuterSizer).   


create_advance_feature(MainPanel, OuterSizer) ->  
   
    Notebook = wxNotebook:new(MainPanel, 1, [{style, ?wxBK_DEFAULT}]),
    Peers = wxBitmap:new("peers.xpm", [{type, ?wxBITMAP_TYPE_XPM}]),
    IL = wxImageList:new(16, 16),
    wxImageList:add(IL, Peers),
    wxNotebook:assignImageList(Notebook, IL),
    
   {CreateText1104, CompleteText1105,  HashText1106} = file_info_page(Notebook),
    TorNameText1201 = torrent_info_page(Notebook),
    peer_page(Notebook), 
    
   
  
    
 
      
    wxSizer:add(OuterSizer, Notebook, [{proportion, 0}, {flag, ?wxEXPAND}]),
    {TorNameText1201, CreateText1104, CompleteText1105, HashText1106}.

file_info_page(Notebook) ->
 Fpage = wxPanel:new(Notebook, []),
    
    FPathText1101 = wxStaticText:new(Fpage, 1101, "Filepath: ", []),
    PText1102 = wxStaticText:new(Fpage, 1102, "Pieces: 4 of 23", []),
    TSizeText1103 = wxStaticText:new(Fpage, 1103, "TotalSize: 65.7MB", []),
    CreateText1104 = wxStaticText:new(Fpage, 1104, "Created on: ", []),
    CompleteText1105 = wxStaticText:new(Fpage, 1105, "Completed on: ", []),
    HashText1106 = wxStaticText:new(Fpage, 1106, "Hash: ", []),
    FileBoxSizer = wxStaticBoxSizer:new(?wxVERTICAL, Fpage, [{label, "File Information"}]),

    FileGridSizer = wxGridSizer:new(4, 2, 5, 5),
   
    wxSizer:add(FileBoxSizer, FileGridSizer, [{proportion, 0}, {flag, ?wxALIGN_LEFT}]),

    
     wxSizer:add(FileGridSizer, FPathText1101, [{proportion, 0}, {flag, ?wxALIGN_LEFT}]),
      wxSizer:add(FileGridSizer, PText1102, [{proportion, 0}, {flag, ?wxALIGN_RIGHT}]),
      wxSizer:add(FileGridSizer,  TSizeText1103, [{proportion, 0}, {flag, ?wxALIGN_LEFT}]), 
      wxSizer:add(FileGridSizer, 30, 10, []),
    wxSizer:add(FileGridSizer, CreateText1104, [{proportion, 0}, {flag, ?wxEXPAND}]),
  
      wxSizer:add(FileGridSizer, CompleteText1105, [{proportion, 0}, {flag, ?wxEXPAND}]), 
      wxSizer:add(FileGridSizer, HashText1106, [{proportion, 0}, {flag, ?wxEXPAND}]),
      wxSizer:add(FileGridSizer, 30, 10, []), 
   
    wxPanel:setSizer(Fpage, FileBoxSizer),
    wxSizer:layout(FileBoxSizer),
    wxNotebook:addPage(Notebook, Fpage, "File Info", []),
    {CreateText1104, CompleteText1105,  HashText1106}.
    


torrent_info_page(Notebook) ->
  
    Tpage = wxPanel:new(Notebook, []),

    TorNameText1201 = wxStaticText:new(Tpage, 1201, "Torrent Name: ", []),
    SeedText1202 = wxStaticText:new(Tpage, 1202, "Seeders: 125", []),
    LeechText1203  = wxStaticText:new(Tpage, 1203, "Leechers: 12", []),
    TpText1204 = wxStaticText:new(Tpage, 1204, "Total Pieces:", []), 
    CpText1205 = wxStaticText:new(Tpage, 1205, "Current number of pieces: ", []),
	TorrentBoxSizer = wxStaticBoxSizer:new(?wxVERTICAL, Tpage, [{label, "Torrent Information"}]),
	TorrentGridSizer = wxGridSizer:new(3, 2, 5, 5),
    wxSizer:add(TorrentBoxSizer, TorrentGridSizer, [{proportion, 0}, {flag, ?wxALIGN_LEFT}]),
		wxSizer:add(TorrentGridSizer, TorNameText1201, [{proportion, 0}, {flag, ?wxALIGN_LEFT}]),
		wxSizer:add(TorrentGridSizer, 30, 10, []),
		wxSizer:add(TorrentGridSizer, SeedText1202, [{proportion, 0}, {flag, ?wxALIGN_LEFT}]),
                wxSizer:add(TorrentGridSizer, LeechText1203, [{proportion, 0}, {flag, ?wxALIGN_RIGHT}]), 
                wxSizer:add(TorrentGridSizer, TpText1204, [{proportion, 0}, {flag, ?wxALIGN_LEFT}]),
                wxSizer:add(TorrentGridSizer, CpText1205, [{proportion, 0}, {flag, ?wxALIGN_RIGHT}]),

    wxPanel:setSizer(Tpage, TorrentBoxSizer ),
    wxNotebook:addPage(Notebook, Tpage, "Torrent Info", []),
    TorNameText1201.
    



peer_page(Notebook) ->
       Page3 = wxPanel:new(Notebook, []),
    wxPanel:setBackgroundColour(Page3, ?wxCYAN),
    PageText3 = wxStaticText:new(Page3, ?wxID_ANY, "This is a Torrent Info tab.",
				[{pos, {50, 100}}]),
    wxStaticText:setForegroundColour(PageText3, ?wxBLACK),
   PageSizer3 = wxBoxSizer:new(?wxHORIZONTAL),
    wxSizer:add(PageSizer3, PageText3),
    wxPanel:setSizer(Page3, PageSizer3),
    wxNotebook:addPage(Notebook, Page3, "Peers", []),
    wxNotebook:setPageImage(Notebook, 2, 0).
    
