-module(exercise2).
-export([joe/0, foobar/1]).

-record(person, {name, age=0, phone}).

joe() -> #person{name="Joe", age=21, phone="999-999"}.

foobar(P) when is_record(P, person), P#person.name == "Joe" ->
    io:format("joe record for: ~p~n", [P]);
foobar(P) ->
    io:format("not joe record: ~p~n", [P]).
