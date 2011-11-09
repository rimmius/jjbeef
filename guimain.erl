%%Programmer: Jody Swartz
%%Description: Main module for the GUI of JJBEEF. Initial trial of first GUI.
%%Created by: 2011-10-18
%%Version 0.01

-module(guimain).
-copyright('Copyight (c) 2011-12 JJBEEF AB').
-compile(export_all).
-include_lib("wx/include/wx.hrl").

%%Start creates the window and the goes straight into the loop.
start() ->
    State = create_window(),
    
    loop (State, createUniqueId()).
%%Creates a unique id:):):)
createUniqueId() ->
    %%check_length(
    {Nr, Nr2, Nr3} = now(),
    check_length(integer_to_list(Nr) ++ integer_to_list(Nr2) ++ integer_to_list(Nr3)).
check_length(Id) when length(Id) =:= 20 ->
    Id;
check_length(Id) ->
    check_length(Id ++ "0").
%%Creates the Window and sets the parameters.
create_window() ->
    GUIServer = wx:new(),  %% Creates the server for the GUI.
    MainFrame = wxFrame:new(GUIServer, -1, "Name Of Product", [{size, {450, 250}}]),
    Panel = wxPanel:new(MainFrame), 
    %%Create  widgets 
    Tex301 = wxTextCtrl:new(Panel, 301, [{value, "Input Here"}]),
    But101 = wxButton:new(Panel, 101, [{label, "&Download"}]),
    Ste201 = wxStaticText:new(Panel, 201, "Display torrent Link", []),
    %%Creates the Sizers
    OuterSpaceSizer = wxBoxSizer:new(?wxHORIZONTAL),
    MasterSizer = wxBoxSizer:new(?wxVERTICAL),
    InputSizer = wxStaticBoxSizer:new(?wxVERTICAL, Panel, [{label, "Please enter the torrent link you wish to download:"}]),
    ButtonSizer = wxBoxSizer:new(?wxHORIZONTAL),
    %%Positions the widgets with the help of the sizers
    wxSizer:add(InputSizer, Tex301, []),
    wxSizer:add(InputSizer, 250, 0, []),
   
    wxSizer:addSpacer(MasterSizer, 20), %%Spacer
    wxSizer:add(MasterSizer, InputSizer, []),
    wxSizer:addSpacer(MasterSizer, 20), %%Spacer
    
    wxSizer:add(MasterSizer, Ste201, []),
    wxSizer:addSpacer(MasterSizer, 10),  %%Spacer
   
    wxSizer:add(ButtonSizer, But101, []),
    wxSizer:add(MasterSizer, ButtonSizer, []),

    wxSizer:addSpacer(OuterSpaceSizer, 75),   %%Spacer
    wxSizer:add(OuterSpaceSizer, MasterSizer, []),
    %%Set MasterSizer into the Panel.
    wxPanel:setSizer(Panel, OuterSpaceSizer),
    wxFrame:show(MainFrame),
    %%create two listeners
    wxPanel:connect(Panel, command_button_clicked),
    %%the return value, which is stored in State
    {Tex301, Ste201}.
loop ({Tex301, Ste201}, Id) ->

    io:format("----Waiting in the loop-- ~n", []), 
    receive
	#wx{id = 101, event=#wxCommand{type = command_button_clicked}} ->
	    Tex301_val = wxTextCtrl:getValue(Tex301),
	    displaymessage(Tex301_val, Ste201),
	    download_manager:start(Tex301_val, self(), Id),
	    loop({Tex301, Ste201}, Id);
	Msg -> 
	    io:format("loop default triggered : Got ~n ~p ~n", [Msg]),
	    Tex301, Ste201,
	    loop({Tex301, Ste201}, Id)
    end.

displaymessage(Torrlink, StatText)  ->
    io:format("~w ~n", [Torrlink]),
    wxStaticText:setLabel(StatText, Torrlink),
    ok.
