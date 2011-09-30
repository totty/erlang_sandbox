-module(mutex).
-export([start/0, stop/0, status/0]).
-export([wait/0, signal/0]).
-export([init/0]).

start() ->
  register(mutex, spawn(?MODULE, init, [])).

stop() ->
  mutex ! stop.

status() ->
  mutex ! status.

wait() ->
  mutex ! {wait, self()},
  receive
    ok -> ok
  end.

signal() ->
  mutex ! {signal, self()},
  ok.

init() ->
  free().

free() ->
  receive
    {wait, Pid} ->
      Pid ! ok,
      busy(Pid);
    status ->
      io:format("mutex status [free].~n", []),
      free();
    stop ->
      terminate()
  end.

busy(Pid) ->
  receive
    {signal, Pid} ->
      free();
    status ->
      io:format("mutex status [busy].~n", []),
      busy(Pid);
    stop ->
      io:format("mutex busy call stop.~n", [])
  end.

terminate() ->
  receive
    {wait, Pid} ->
      io:format("mutex terminate call wait.~n", []),
      exit(Pid, kill),
      terminate();
    status ->
      io:format("mutex status [terminating].~n", []),
      terminate()
  after
    10000 ->
      %% 1000 > 1sec, 10000 > 10sec
      io:format("mutex terminate call after.~n", []),
      ok
  end.


