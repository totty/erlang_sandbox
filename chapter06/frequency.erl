-module(frequency).
-export([start/0, stop/0, status/0]).
-export([allocate/0, deallocate/1]).
-export([init/0]).

start()  -> register(frequency, spawn(frequency, init, [])).
stop()   -> call(stop).
status() -> call(status).

init() ->
    process_flag(trap_exit, true),
    Frequencies = {get_frequencies(), []},
    loop(Frequencies),
    ok.

get_frequencies() -> [10,11,12,13,14,15].

allocate()        -> call(allocate).
deallocate(Freq)  -> call({deallocate, Freq}).

call(Message) ->
    frequency ! {request, self(), Message},
    receive
        {reply, Reply} -> Reply
    end.

reply(Pid, Message) -> Pid ! {reply, Message}.

loop(Frequencies) ->
    receive
        {request, Pid, allocate} ->
            io:format("allocate~n", []),
            {NewFrequencies, Reply} = allocate(Frequencies, Pid),
            reply(Pid, Reply),
            loop(NewFrequencies);
        {request, Pid, {deallocate, Freq}} ->
            io:format("deallocate~n", []),
            {NewFrequencies, Reply} = deallocate(Frequencies, Freq, Pid),
            reply(Pid, Reply),
            loop(NewFrequencies);
        {request, Pid, status} ->
            io:format("status [~w]~n", [Frequencies]),
            reply(Pid, ok),
            loop(Frequencies);
        {request, Pid, stop} ->
            io:format("stop~n", []),
            reply(Pid, ok);
        {'EXIT',  Pid, Reason} ->
            io:format("exit~n", []),
            NewFrequencies = exited(Frequencies, Pid),
            reply(Pid, ok),
            loop(NewFrequencies)
    end.

allocate({[                  ], Allocated}, _Pid) ->
    {{[], Allocated}, {error, no_frequencies}};
allocate({[Freq | Frequencies], Allocated},  Pid) ->
    link(Pid),
    {{Frequencies, [{Freq, Pid} | Allocated]}, {ok, Freq}}.

deallocate({Free, Allocated}, Freq, Pid) ->
    case lists:keysearch(Freq, 1, Allocated) of
        {value, {Freq, _Pid}} ->
            %% exit の後には、self() が変わるので、allocated した際に Pid は変わっている
            unlink(Pid),
            NewAllocated = lists:keydelete(Freq, 1, Allocated),
            {{[Freq | Free], NewAllocated}, {ok, Freq}};
        false ->
            {{Free, Allocated}, {error, not_frequency_error}}
    end.

exited({Free, Allocated}, Pid) ->
    case lists:keysearch(Pid, 2, Allocated) of
        {value, {Freq, Pid}} ->
            NewAllocated = lists:keydelete(Freq, 1, Allocated),
            {[Freq | Free], NewAllocated};
        false ->
            {Free, Allocated}
    end.
