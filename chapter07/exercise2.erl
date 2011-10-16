-module(exercise2).
-export([joe/0, foobar/1]).

-record(person, {name, age=0, phone}).

joe() -> #person{name="Joe", age=21, phone="999-999"}.

%% foobar(#person{name=Name, age=Age} = P) when Record(P, person) ->
%%     io:format("name: ~p, age: ~p, phone: ~p~n", [Name, Age, Phone]);
foobar(P) when is_record(P, person) ->
    io:format("record for: ~p~n", [P]);
foobar(P) ->
    io:format("not record: ~p~n", [P]).
