-module(log_handler).
-export([init/1, terminate/1, handle_event/2]).

init({swap, Fd}) ->
  io:format("log_handler init swap mode", []),
  ok = file:sync(Fd),
  Fd;
init(File) ->
  io:format("log_handler init normal mode", []),
  {ok, Fd} = file:open(File, write),
  Fd.

terminate({swap, Fd}) ->
  io:format("log_handler terminate swap mode", []),
  Fd;
terminate(Fd) ->
  io:format("log_handler terminate normal mode", []),
  file:close(Fd).

handle_event({Action, Id, Event}, Fd) ->
  {MegaSec, Sec, MicroSec} = now(),
  % Args = io:format(Fd, "~w,~w,~w,~w,~w,~p~n", [MegaSec, Sec, MicroSec, Action, Id, Event]),
  io:format(Fd, "~w,~w,~w,~w,~w,~p~n", [MegaSec, Sec, MicroSec, Action, Id, Event]),
  Fd;
handle_event(_, Fd) -> Fd.

