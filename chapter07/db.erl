-module(db).
-export([new/0, destroy/1, write/3, delete/2, read/2, match/2]).
-include("db.hrl").

new() -> [].

destroy(_) -> ok.

write(Key, Element, Db) ->
  [#data{key=Key, data=Element} | Db].

delete(Key, Db) ->
  delete_acc(Key, Db, []).

delete_acc(_, [], Acc) ->
  Acc;
delete_acc(Key, [#data{key=Key} | Data], Acc) ->
  delete_acc(Key, Data, Acc);
delete_acc(Key, [P | Data], Acc) ->
  delete_acc(Key, Data, [P | Acc]).

read(_, []) ->
  {error, instance};
read(Key, [#data{key=Key} = P | _]) ->
  {ok, P};
read(Key, [_ | Data]) ->
  read(Key, Data).

match(Element, Db) ->
  match_acc(Element, Db, []).

match_acc(_, [], Acc) ->
  Acc;
match_acc(Element, [#data{data=Element} = P | Data], Acc) ->
  match_acc(Element, Data, [P | Acc]);
match_acc(Element, [_ | Data], Acc) ->
  match_acc(Element, Data, Acc).
