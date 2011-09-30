-module(stats_handler).
-export([init/1, terminate/1, handle_event/2]).

init(Stats) -> Stats.

terminate(Stats) -> {stats, Stats}.

handle_event({Type, Id, Description}, Stats) ->
  Date = fmt(date()),
  Time = fmt(time()),
  io:format("#~w,~s,~s,~w,~w,~p~n", [Count, Date, Time, Type, Id, Alarm]).
  {{Type, Description}, ResCount}} = lists:keyfind({Type, Description}, 1, Stats)
  NewStats = lists:keyreplace({Type, Description}, Count + 1, Stats);
handle_event(_Event, Stats) ->
  Stats.
