-module(event_manager).
-export([start/2, stop/1]).
-export([add_handler/3, delete_handler/2, get_data/2, send_event/2, swap_handlers/3]).
-export([init/1]).

start(Name, HandlerList) ->
  register(Name, spawn(event_manager, init, [HandlerList])), ok.

init(HandlerList) ->
  loop(initialize(HandlerList)).

initialize([]) -> [];
initialize([{Handler, InitData} | Rest]) ->
  [{Handler, Handler:init(InitData)} | initialize(Rest)].

stop(Name) ->
  Name ! {stop, self()},
  receive
    {reply, Reply} ->
      Reply
  end.

terminate([]) -> [];
terminate([{Handler, Data} | Rest]) ->
  [{Handler, Handler:terminate(Data)} | terminate(Rest)].

add_handler(Name, Handler, InitData) ->
  call(Name, {add_handler, Handler, InitData}).

delete_handler(Name, Handler) ->
  call(Name, {delete_handler, Handler}).

swap_handlers(Name, OldHandler, NewHandler) ->
  call(Name, {swap_handlers, [OldHandler, NewHandler]}).

get_data(Name, Handler) ->
  call(Name, {get_data, Handler}).

send_event(Name, Event) ->
  call(Name, {send_event, Event}).

handler_msg({add_handler, Handler, InitData}, LoopData) ->
  {ok, [{Handler, Handler:init(InitData)} | LoopData]};

handler_msg({delete_handler, Handler}, LoopData) ->
  case lists:keysearch(Handler, 1, LoopData) of
    false -> {{error, instance}, LoopData};
    {value, {Handler, Data}} ->
      Reply = {data, Handler:terminate(Data)},
      NewLoopData = lists:keydelete(Handler, 1, LoopData),
      {Reply, NewLoopData}
  end;

handler_msg({swap_handlers, [OldHandler, NewHandler]}, LoopData) ->
  case lists:keysearch(OldHandler, 1, LoopData) of
    false -> {{error, instance}, LoopData};
    {value, {OldHandler, Data}} ->
      io:format("1", []),
      OldData = OldHandler:terminate({swap, Data}),
      io:format("2", []),
      NewData = NewHandler:init(OldData),
      io:format("3", []),
      SwapLoopData = [NewData | lists:keydelete(OldHandler, 1, LoopData)],
      io:format("4", []),
			{ok, [{NewHandler, SwapLoopData} | LoopData]}
  end;

handler_msg({get_data, Handler}, LoopData) ->
  case lists:keysearch(Handler, 1, LoopData) of
    false -> {{error, instance}, LoopData};
    {value, {Handler, Data}} -> {{data, Data}, LoopData}
  end;

handler_msg({send_event, Event}, LoopData) ->
  {ok, event(Event, LoopData)}.

event(_Event, []) -> [];
event(Event, [{Handler, Data} | Rest]) ->
  [{Handler, Handler:handle_event(Event, Data)} | event(Event, Rest)].

%% 以下、鉄板

call(Name, Msg) ->
  Name ! {request, self(), Msg},
  receive
    {reply, Reply} -> Reply
  end.

reply(To, Msg) ->
  To ! {reply, Msg}.

loop(State) ->
  receive
    {request, From, Msg} ->
      {Reply, NewState} = handler_msg(Msg, State),
      reply(From, Reply),
      loop(NewState);
    {stop, From} ->
      reply(From, terminate(State))
  end.

