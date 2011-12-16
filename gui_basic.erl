-module(gui_basic).

-include_lib("wx/include/wx.hrl").

-export([start/0]).
-compile(export_all).
-define(ADVANCE_FUNCTIONS_START, 1001).
-define(ADVANCE_FUNCTIONS_END, 1002).

start() ->
    State = create_window(),
    process_flag(trap_exit, true),
    loop(State, createUniqueId()).

%%Creates a unique id:):):)
createUniqueId() ->
    %%check_length(
    
    check_length("-JB1010-").
check_length(Id) when length(Id) =:= 20 ->
    Id;
check_length(Id) ->
    check_length(Id ++ integer_to_list(random:uniform(9))).

create_window() ->
    Wx = wx:new(),
    Frame = wxFrame:new(Wx, 
			-1, % window id
			"Glögg - BitTorrent Downloader ", % window title
			[{size, {600, 180}}, {style, ?wxDEFAULT_FRAME_STYLE band bnot ?wxRESIZE_BORDER band bnot ?wxMAXIMIZE_BOX}]),
    %% ICON for Frame
    Icon = wxIcon:new("glogg.ico", [{type, ?wxBITMAP_TYPE_ICO}]),
    wxFrame:setIcon(Frame, Icon),
    
    StatusBar =  wxFrame:createStatusBar(Frame,[{style, ?wxSB_RAISED}]),
    wxStatusBar:setFieldsCount(StatusBar, 4), 
    wxFrame:connect(Frame, close_window),
    wxTopLevelWindow:setSizeHints(Frame, 600, 180, [{maxW, 600}, {maxH, 500}]),
    
    
    
    %%ToolBar
    ToolBar = wxFrame:createToolBar(Frame,[]),
    Open = wxBitmap:new("open.xpm", [{type, ?wxBITMAP_TYPE_XPM}]),
    Help = wxBitmap:new("help.xpm", [{type, ?wxBITMAP_TYPE_XPM}]),
    Info = wxBitmap:new("info.xpm", [{type, ?wxBITMAP_TYPE_XPM}]),
    Advance_Start = wxBitmap:new("config1.xpm", [{type, ?wxBITMAP_TYPE_XPM}]),
    Advance_End = wxBitmap:new("config2.xpm", [{type, ?wxBITMAP_TYPE_XPM}]),
    
    wxToolBar:insertSeparator(ToolBar, 0),
    wxToolBar:addTool(ToolBar, ?wxID_OPEN, "Open",  Open, [{shortHelp, "Open"}]),
    wxToolBar:setToolLongHelp(ToolBar, ?wxID_OPEN, "Open's a torrent"),
    wxToolBar:insertSeparator(ToolBar, 2),  
    wxToolBar:addCheckTool(ToolBar, ?ADVANCE_FUNCTIONS_END, "Advance", Advance_Start, [{shortHelp, "Advance"}, {longHelp, "Activate Advance Function"}]),
 
    wxToolBar:insertSeparator(ToolBar, 4),
    wxToolBar:addTool(ToolBar, ?wxID_HELP, "Help", Help , [{shortHelp, "Help"}]),   
    wxToolBar:setToolLongHelp(ToolBar, ?wxID_HELP, "Help function"),
    wxToolBar:insertSeparator(ToolBar, 6),
    
    wxToolBar:addTool(ToolBar, ?wxID_ABOUT, "About", Info, [{shortHelp, "Info"}]),
    wxToolBar:setToolLongHelp(ToolBar, ?wxID_ABOUT, "Application Information"),
    wxToolBar:insertSeparator(ToolBar, 8),
    
    wxToolBar:realize(ToolBar),
    wxFrame:setToolBar(Frame, ToolBar),
    
    
    
    
    MainPanel = wxPanel:new(Frame, [{style, ?wxFULL_REPAINT_ON_RESIZE}]),
    wxWindow:setBackgroundColour(MainPanel, ?wxWHITE),
    OuterSizer = wxGridSizer:new(2, 1, 2, 2),
    
    BasicSizer = wxBoxSizer:new(?wxVERTICAL),
    
    wxSizer:add(OuterSizer, BasicSizer, [{proportion, 0}, {flag, ?wxEXPAND}]),
    
    
    DL_Panel = wxPanel:new(MainPanel, [{winid, ?wxID_ANY}, {size, {100, 175}}, {style, ?wxFULL_REPAINT_ON_RESIZE}]),
    
						%Download Icon
    Image_Dl = wxImage:new("download.png", [{type, ?wxBITMAP_TYPE_PNG}]),
    Bitmap_Dl = wxBitmap:new(Image_Dl, []),
    StaticBitmap_Dl  = wxStaticBitmap:new(DL_Panel, 2, Bitmap_Dl),
    wxStaticBitmap:setToolTip(StaticBitmap_Dl, "Downloading Torrent"),
    wxWindow:show(StaticBitmap_Dl, [{show, false}]),
    
    
    
    %%Filename Text
    Text = wxStaticText:new(DL_Panel, 1, "Filename", [{style, ?wxALIGN_LEFT bor ?wxST_NO_AUTORESIZE}]),

    %%Time Text
    TimeText  = wxStaticText:new(DL_Panel, 3, "Time ", [{style, ?wxALIGN_LEFT bor ?wxST_NO_AUTORESIZE}]),
    
    
						%Folder ICON
    Image_Folder = wxImage:new("folder.png", [{type, ?wxBITMAP_TYPE_PNG}]),
    Bitmap_Folder = wxBitmap:new(Image_Folder, []),
    StaticBitmap_Folder  = wxStaticBitmap:new(DL_Panel, 1, Bitmap_Folder),
    
    
    
    
    
    %%Gauge
    Range=100,
    NormalGauge = wxGauge:new(DL_Panel, 1, Range, [{pos, {167, 45}}, {size, {200, -1}}, {style, ?wxGA_HORIZONTAL}]),
    
    
    
    
    
    
						%Cancel Icon
    Image_Cancel = wxImage:new("delete.png", [{type, ?wxBITMAP_TYPE_PNG}]),
    Bitmap_Cancel = wxBitmap:new(Image_Cancel, []),
    %%StaticBitmap_Cancel  = wxStaticBitmap:new(DL_Panel, 3, Bitmap_Cancel),
    But101 = wxBitmapButton:new(DL_Panel, 101, Bitmap_Cancel),
    wxButton:setToolTip(But101, "Cancel Download"),
    wxWindow:disable(But101),
    
    
    
    %%Calculation Text  
    CalcText  = wxStaticText:new(DL_Panel, 2, "Complete ", [{style, ?wxALIGN_LEFT bor ?wxST_NO_AUTORESIZE}]),
    
    %%URL Text   
    TrackText = wxStaticText:new(DL_Panel, 3, " - URL ", [{style, ?wxALIGN_LEFT bor ?wxST_NO_AUTORESIZE}]),
    
    StatGridSizer = wxGridSizer:new(1, 2, 1, 1), 
    
    
    wxSizer:add(StatGridSizer, CalcText, [{proportion, 0}, {flag, ?wxEXPAND}]),
    wxSizer:add(StatGridSizer, TrackText, [{proportion, 0}, {flag, ?wxEXPAND}]),    
    wxSizer:layout(StatGridSizer), 
    
    %%Sizers 
    FlexGridSizer = wxFlexGridSizer:new(4, 3, 2, 2), 
    wxSizer:add(FlexGridSizer, 25, 15, []), 
    wxSizer:add(FlexGridSizer, 25, 15, []),   
    wxSizer:add(FlexGridSizer, 25, 15, []),
    wxSizer:add(FlexGridSizer, StaticBitmap_Dl, [{proportion, 0}, {flag, ?wxALIGN_CENTER}]),  
    wxSizer:add(FlexGridSizer, Text, [{proportion, 0}, {flag, ?wxEXPAND}]),
    wxSizer:add(FlexGridSizer, TimeText, [{proportion, 0}, {flag, ?wxALIGN_CENTER}]),
    wxSizer:add(FlexGridSizer, StaticBitmap_Folder , [{proportion, 0},{flag, ?wxALIGN_CENTER}]),
    wxSizer:add(FlexGridSizer, NormalGauge, [{proportion, 0}, {flag, ?wxEXPAND}]),
    wxSizer:add(FlexGridSizer, But101, [{proportion, 0}, {flag, ?wxALIGN_CENTER}]),
    wxSizer:add(FlexGridSizer, 50, 30, []),
    wxSizer:add(FlexGridSizer, StatGridSizer, [{proportion, 0}, {flag, ?wxEXPAND}]),
    wxSizer:add(FlexGridSizer, 50, 30, []),

    wxFlexGridSizer:addGrowableCol(FlexGridSizer, 1),   
    
    wxSizer:add(BasicSizer,  DL_Panel, [{proportion, 0}, {flag, ?wxEXPAND}]),
     Advance_sizer= wxBoxSizer:new(?wxVERTICAL),
   wxSizer:add(OuterSizer, Advance_sizer,[{proportion, 0}, {flag, ?wxEXPAND}]), 
    
    
  
    
    wxSizer:layout(FlexGridSizer),
    wxSizer:layout(BasicSizer),
    wxSizer:layout(OuterSizer),
   wxPanel:setSizer(DL_Panel, FlexGridSizer),
 {TorNameText1201, CreateText1104, CompleteText1105,  HashText1106} =  gui_advance:start(MainPanel, OuterSizer),
 wxPanel:setSizer(MainPanel, OuterSizer),
    
     wxToolBar:connect(ToolBar,  command_menu_selected, []),
     wxToolBar:connect(ToolBar, command_left_click),
     wxPanel:connect(MainPanel, command_button_clicked),
   

     
    ok = wxFrame:setStatusText(Frame, "Welcome to Glögg!", [{number, 0}]),
    wxWindow:show(Frame),
    {Frame, StatusBar, ToolBar, Text, NormalGauge, CalcText, TimeText, TrackText, Advance_Start, Advance_End, TorNameText1201, CreateText1104, CompleteText1105, But101, StaticBitmap_Dl, HashText1106}.


loop(State, Download_pid) ->
    {Frame, StatusBar, ToolBar, Text, NormalGauge, CalcText, TimeText, TrackText, Advance_Start, Advance_End, TorNameText1201, CreateText1104, CompleteText1105, But101, StaticBitmap_Dl,  HashText1106} = State,
   
    receive 
   	#wx{event=#wxClose{}} ->
   	    io:format("~p Closing window ~n",[self()]),
   	    ok = wxFrame:setStatusText(Frame, "Closing...",[]),
	    wxWindow:destroy(State), 
	    ok;
        
	#wx{id= ?wxID_OPEN, event=#wxCommand{type=command_menu_selected}} ->
	    io:format("The Open button works!~n"),
	    
	    FC = wxFileDialog:new(Frame, [{style, ?wxFD_OPEN}]),
	    case wxDialog:showModal(FC) of  
		?wxID_OK ->
		    Path = wxFileDialog:getPath(FC),
		    Fname2 = filename:basename(Path),
		    
		    case string:str(Fname2, ".torrent") of
			0  ->
			    io:format("Wrong File has been chosen! ~n"),
			    Error1 =  wxMessageDialog:new(Frame, "Invalid File Selection", [{caption, "Invalid File"}, {style, ?wxICON_ERROR}]), 
			    wxMessageDialog:showModal(Error1),
                  
			    loop(State, Download_pid);       
			_ -> 
			    
			    DownloadPid_new = download_manager:start(Path, self()),
			    link(DownloadPid_new),
                          
                            wxWindow:show(StaticBitmap_Dl, [{show, true}]), 
                            wxWindow:refresh(Frame),
                            wxWindow:enable(But101),
			    wxStaticText:setLabel(Text, Fname2),
			    wxToolBar:enableTool(ToolBar, ?wxID_OPEN, false),
			    Fname1 = "Filename: ",
			    TorrName  = Fname1 ++ Fname2,
                            wxStaticText:setLabel(TorNameText1201, TorrName),
                            
                            wxStaticText:setLabel(TimeText, create_time()),
                            Create_string = "Created on: " ++ create_date_and_time(),                       
                           wxWindow:refresh(Frame),
                            wxStaticText:setLabel(CreateText1104, Create_string),                          wxWindow:refresh(Frame),
			    wxStatusBar:setStatusText(StatusBar, "Download Started...", [{number, 0}]), 
			    wxStatusBar:setStatusText(StatusBar, TorrName, [{number, 1}]),
			    loop(State, DownloadPid_new)

		    end;
		_ ->
		    ignore
	    end,
	    
	    wxFileDialog:destroy(FC),
	    loop(State, Download_pid);
	

	#wx{id= ?ADVANCE_FUNCTIONS_START, event=#wxCommand{type=command_menu_selected}} ->
	    io:format("Advance functions button works! \n"),
	    
	    
	    wxToolBar:removeTool(ToolBar, ?ADVANCE_FUNCTIONS_START), 
	    wxToolBar:insertTool(ToolBar, 3, ?ADVANCE_FUNCTIONS_END, "Advance", Advance_End, [{shortHelp, "Advance"}, {longHelp, "Activate Advance Function"}]),
	    io:format("toggle set to : true\n"),   
	    wxToolBar:realize(ToolBar),
            wxTopLevelWindow:setSize(Frame, 600, 190), 
	    loop(State, Download_pid);
	    
	
	#wx{id= ?ADVANCE_FUNCTIONS_END, event=#wxCommand{type=command_menu_selected}} ->	 
	    
	    wxToolBar:removeTool(ToolBar, ?ADVANCE_FUNCTIONS_END), 
	    wxToolBar:insertTool(ToolBar, 3, ?ADVANCE_FUNCTIONS_START, "Advance", Advance_Start, [{shortHelp, "Advance"}, {longHelp, "Activate Advance Function"}]),
	    
	    io:format("toggle set to : false\n"),       
	    wxToolBar:realize(ToolBar),
	    wxTopLevelWindow:setSize(Frame, 600, 500),
	    loop(State, Download_pid);								       
	
	
	{hash, Hash} ->
	    wxStaticText:setLabel(HashText1106, Hash),
         wxWindow:refresh(Frame),
          loop(State, Download_pid);
	    
	{tracker, Track} ->
	    wxStaticText:setLabel(TrackText, Track),
	    wxWindow:refresh(Frame),
 	    loop(State, Download_pid);
	
	
	
	
        {percentage, Percent} ->
	    wxGauge:setValue(NormalGauge, Percent),
	    PText1 =  lists:concat([Percent]),
	    PText2 =   "% Complete",
            StText = PText1 ++ PText2,
	    wxStaticText:setLabel(CalcText, StText),
	    wxStatusBar:setStatusText(StatusBar, StText, [{number, 3}]),
	    case  wxGauge:getValue(NormalGauge) of 
		100 ->
                     Date_time_string = "Completed on: " ++ create_date_and_time(),
		    wxStaticText:setLabel(CompleteText1105, Date_time_string),               
		    Complete_dialog =  wxMessageDialog:new(Frame, "Succesfull Download!", [{caption, "Congratulations"}, {style, ?wxICON_EXCLAMATION}]), 
		    wxMessageDialog:showModal(Complete_dialog);
	       
		
                
		
		_ ->
		    ignore
	    end,
	    wxWindow:refresh(Frame),
            loop(State, Download_pid);
	
	
	{'EXIT', _Pid, _Reason} ->
	    wxStatusBar:setStatusText(StatusBar, "Cannot connect to Torrent Tracker", [{number, 2}]),
	    Error2 =  wxMessageDialog:new(Frame, "Cannot connect to Torrent Tracker", [{caption, "Connection timed out"}, {style, ?wxYES_NO}]),        
	    case wxMessageDialog:showModal(Error2) of
		?wxID_YES ->
		    io:format("The Yes Button works! ~n"),            
		    loop(State, Download_pid);	  
		?wxID_NO ->
		    wxToolBar:enableTool(ToolBar, ?wxID_OPEN, true),
		    wxStatusBar:setStatusText(StatusBar, " ", [{number, 1}]),
		    wxStatusBar:setStatusText(StatusBar, " ", [{number, 2}]),
		    wxStatusBar:setStatusText(StatusBar, "Welcome to Glögg!", [{number, 0}])
	    end;
	
	#wx{id = 101, event=#wxCommand{type = command_button_clicked}} ->
	    io:format("Torrent download deleted ~n"),
	    wxGauge:setValue(NormalGauge, 0),
	    wxToolBar:enableTool(ToolBar, ?wxID_OPEN, true),
	    wxStaticText:setLabel(CalcText, " "),
	    wxStaticText:setLabel(Text, " "),
	    wxStaticText:setLabel(TimeText, " "),
	    wxStatusBar:setStatusText(StatusBar, " ", [{number, 1}]),
	    wxStatusBar:setStatusText(StatusBar, " ", [{number, 2}]),
	    wxStatusBar:setStatusText(StatusBar, "Welcome to Glögg!", [{number, 0}]),
	    case Download_pid of
		not_started ->
		    loop(State, not_started);
		_ ->
		    spawn(fun() -> close_for_now(Download_pid) end),
		    loop(State, not_started)
	    end;
	
	
	#wx{id= ?wxID_HELP, event=#wxCommand{type=command_menu_selected}} ->
	    io:format("The HELP icon works!"),
	    wx_misc:launchDefaultBrowser("http://jjbeef.yolasite.com/"),
	    loop(State, Download_pid);
	
	
	
	#wx{id= ?wxID_ABOUT, event=#wxCommand{type=command_menu_selected}} ->
	    gui_info:start(Frame),
	    loop(State, Download_pid);
	
	
	
	Msg ->
	    io:format("Got ~p ~n", [Msg]),
	    State,
            loop(State, Download_pid)
		
    end.


close_for_now(Download_pid) ->
    receive
    after 1000 ->
	    Download_pid ! stop
    end.


create_time() ->
    {H,M,_} = erlang:time(),
    TText = integer_to_list(H) ++ ":" ++ integer_to_list(M),
    TText.
   

create_date_and_time() ->     
    {H,M,S} = erlang:time(),
    {Y,Mo,D} = erlang:date(),
    DText = integer_to_list(Y) ++ "-" ++ integer_to_list(Mo) ++ "-" ++ integer_to_list(D),
    FText = integer_to_list(H) ++ ":" ++ integer_to_list(M) ++ ":" ++ integer_to_list(S),
    Date_and_time = "Date: " ++ DText ++ "  " ++ "Time: " ++ FText ++ " ", 
    Date_and_time.


