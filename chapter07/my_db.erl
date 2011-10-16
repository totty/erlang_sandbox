-module(my_db).
-export([start/0, stop/0, write/2, delete/1, read/1, match/1]).
-export([init/0]).
-include("debug.hrl").

start() ->
  register(my_db, spawn(my_db, init, [])),
  ok.

init() ->
  db:new(),
  loop(db:new()).

stop() ->
  my_db ! stop, ok.

write(Key, Element) ->
  my_db ! {write, self(), [Key, Element]},
  ok.

delete(Key) -> 
  my_db ! {delete, self(), [Key]},
  ok.

read(Key) ->
  my_db ! {read, self(), [Key]},
  receive
    Result -> Result
  end.

match(Element) ->
  my_db ! {match, self(), [Element]},
  receive
    Result -> Result
  end.

loop(Db) ->
  receive
    stop ->
      ?DEBUG(stop, []),
      ok;
    {write, _Pid, [Key, Element]} ->
      ?DEBUG(write, [Key, Element]),
      loop(db:write(Key, Element, Db));
    {delete, _Pid, [Key]} ->
      ?DEBUG(delete, [Key]),
      loop(db:delete(Key, Db));
    {read, Pid, [Key]} ->
      ?DEBUG(read, [Key]),
      Pid ! db:read(Key, Db),
      loop(Db);
    {match, Pid, [Element]} ->
      ?DEBUG(match, [Element]),
      Pid ! db:match(Element, Db),
      loop(Db)
  end.
