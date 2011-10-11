-module(macros1).
-export([test1/0]).

-define(VALUE(Call), io:format("~p = ~p~n", [??Call, Call])).

test1() ->
    ?VALUE(length([1,2,3])).
