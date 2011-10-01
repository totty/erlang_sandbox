-module(add_one).
-export([start/0, stop/0, request/1, loop/0]).

start() ->
  register(add_one, spawn_link(?MODULE, loop, [])).

stop() ->
  add_one ! stop.

request(Int) ->
  add_one ! {request, self(), Int},
  receive
    {result, Result} -> Result
  after
    % 1sec
    1000 -> timeout
  end.

loop() ->
  receive
    {request, Pid, Msg} ->
      Pid ! {result, Msg + 1},
      loop();
    stop ->
      ok
  end.
