-module(tuples1).
-export([test1/0, test2/0]).

birthday({Name, Age, Phone}) -> {Name, Age+1, Phone}.

joe() -> {"Joe", 21, "999-999"}.

showPerson({Name, Age, Phone}) ->
    io:format("name: ~p age: ~p phone: ~p~n", [Name, Age, Phone]).

test1() -> showPerson(joe()).
test2() -> showPerson(birthday(joe())).
