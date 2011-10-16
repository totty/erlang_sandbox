-ifdef(show).
  % c(exercise6,[{d,show}]).
  -define(SHOW_EVAL(Value), io:format("~p = ~p~n", [??Value, Value])).
-else.
  % c(exercise6).
  -define(SHOW_EVAL(Value), ok).
-endif.
