-ifdef(debug).
  % c(MODULE,[{d,debug}]).
  -define(DEBUG(Action, Request), io:format("~p: ~p = ~p~n", [Action, ??Request, Request])).
-else.
  % c(MODULE).
  -define(DEBUG(Action, Request), ok).
-endif.
