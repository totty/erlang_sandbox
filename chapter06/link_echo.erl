-module(link_echo).
-export([start/0, init/0, print/1, stop/0]).

start() ->
    register(link_echo, spawn_link(?MODULE, init, [])),
    ok.

init() ->
    process_flag(trap_exit, true),
    loop().

loop() ->
    receive
        {print, Term} ->
            io:format("[print ~w] ~p.~n", [self(), Term]),
            loop();
        {'EXIT', Pid, Reason} ->
            io:format("[exit ~w] ~p.~n", [Pid, Reason]),
            ok;
        3000 ->
            io:format("[timeout ~w].~n", [self()])
    end.

print(Term) ->
  link_echo ! {print, Term}.

stop() ->
    exit(self(), kill).
