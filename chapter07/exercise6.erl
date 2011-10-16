-module(exercise6).
-export([eval/1]).
-include("exercise6.hrl").

eval(Value) ->
    ?SHOW_EVAL(Value),
    Value.

