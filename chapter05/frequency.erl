-module(frequency).
-export([start/0, stop/0, status/0]).
-export([allocate/0, deallocate/1]).
-export([init/0]).

%% These are the start functions used to create and initialize the server.

start() -> 
  register(frequency, spawn(frequency, init, [])), ok.

init() ->
  Frequencies = {get_frequencies(), []},
  loop(Frequencies).

% Hard Coded
get_frequencies() -> [10,11,12,13,14,15].

stop() -> call(stop).

status() -> call(status).

%% The client Functions

allocate() -> call(allocate).
deallocate(Freq) -> call({deallocate, Freq}).

%% We hide all message passing and the message protocol in a functional interface.

call(Message) ->
  frequency ! {request, self(), Message},
  receive
   {reply, Reply} -> Reply
  end.

%% The Main Loop

loop(Frequencies) ->
  receive
    {request, Pid, allocate} ->
      {NewFrequencies, Reply} = allocate(Frequencies, Pid),
      reply(Pid, Reply),
      loop(NewFrequencies);
    {request, Pid, {deallocate, Freq}} ->
      NewFrequencies = deallocate(Frequencies, Freq, Pid),
      reply(Pid, ok),
      loop(NewFrequencies);
    {request, Pid, status} ->
      io:format("frequencies [~w]~n", [Frequencies]),
      reply(Pid, ok),
      loop(Frequencies);
    {request, Pid, stop} ->
      reply(Pid, ok)
  end.

reply(Pid, Reply) -> Pid ! {reply, Reply}.

%% The Internal Help Functions used to allocate and deallocate frequencies.

allocate({[], Allocated}, _Pid) ->
  {{[], Allocated}, {error, no_frequencies}};
allocate({[UseFreq | FreeFreq], Allocated}, Pid) ->
  {{FreeFreq, [{UseFreq, Pid} | Allocated]}, {ok, UseFreq}}.

deallocate({FreeFreq, Allocated}, UsedFreq, Pid) ->
%   % guard
%   case lists:keymember(UsedFreq, 1, Allocated) of
%     true ->
%       NewAllocated = lists:keydelete(UsedFreq, 1, Allocated),
%       {[UsedFreq | FreeFreq], NewAllocated};
%     false -> {FreeFreq, Allocated}
%   end.
  case lists:member({UsedFreq, Pid}, Allocated) of
    true ->
      NewAllocated = lists:delete({UsedFreq, Pid}, Allocated),
      {[UsedFreq | FreeFreq], NewAllocated};
    false -> {FreeFreq, Allocated}
  end.
