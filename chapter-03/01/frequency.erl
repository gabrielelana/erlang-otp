% Example of frequency server used in chapter 03 as a starting point to
% separate specif from generic behaviour
%
% compile: erlc chapter-03/01/frequency.erl
% check: dialyzer chapter-03/01/frequency.erl
%
% after compile, use: erl
% > frequency:start().
% > true
% > frequency:alocate().
% > {ok, 10}
% > frequency:alocate().
% > {ok, 11}
% > frequency:alocate().
% > {ok, 12}
% > frequency:deallocate(10).
% > ok
% > frequency:stop().
% > ok

-module(frequency).
-export([start/0, stop/0, allocate/0, deallocate/1]).
-export([init/0]).
-export_type([frequency/0]).

-type frequency() :: integer().
-type allocated_frequency() :: {frequency(), pid()}.
-type loop_data() :: {[frequency()], [allocated_frequency()]}.

%% client API

-spec start() -> true.
start() ->
  register(frequency, spawn(frequency, init, [])).

-spec stop() -> ok.
stop() ->
  call(stop).

-spec allocate() -> {ok, frequency()} | {error, no_frequency}.
allocate() ->
  call(allocate).

-spec deallocate(frequency()) -> ok.
deallocate(Frequency) ->
  call({deallocate, Frequency}).


%% callback API

-spec init() -> any().
init() ->
  Frequencies = {possible_frequencies(), []},
  loop(Frequencies).


%% generic

call(Message) ->
  frequency ! {request, self(), Message},
  receive
    {reply, Reply} -> Reply
  end.

reply(Pid, Reply) ->
  Pid ! {reply, Reply}.

loop(Frequencies) ->
  receive
    {request, Pid, allocate} ->
      {UpdatedFrequencies, Reply} = allocate(Frequencies, Pid),
      reply(Pid, Reply),
      loop(UpdatedFrequencies);
    {request, Pid, {deallocate, Frequency}} ->
      UpdatedFrequencies = deallocate(Frequencies, Frequency),
      reply(Pid, ok),
      loop(UpdatedFrequencies);
    {request, Pid, stop} ->
      reply(Pid, ok)
  end.


%% private

-spec allocate(loop_data(), pid()) -> {loop_data(), {ok, frequency()} | {error, no_frequency}}.
allocate(Frequencies = {[], _Allocated}, _Pid) ->
  {Frequencies, {error, no_frequency}};
allocate({[Frequency|Free], Allocated}, Pid) ->
  {{Free, [{Frequency, Pid} | Allocated]}, {ok, Frequency}}.

-spec deallocate(loop_data(), frequency()) -> loop_data().
deallocate({Free, Allocated}, Frequency) ->
  AllocatedAfterAllocation = lists:keydelete(Frequency, 1, Allocated),
  {[Frequency|Free], AllocatedAfterAllocation}.

-spec possible_frequencies() -> [frequency()].
possible_frequencies() ->
  [10, 11, 12, 13, 14, 15].
