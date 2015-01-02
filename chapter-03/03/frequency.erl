-module(frequency).
-export([start/0, stop/0, allocate/0, deallocate/1]).
-export([init/0, terminate/1, handle/2]).
-export_type([frequency/0]).

-type frequency() :: integer().
-type allocated_frequency() :: {frequency(), pid()}.
-type frequencies() :: {[frequency()], [allocated_frequency()]}.


% client API

-spec start() -> true.
start() ->
  server:start(frequency, []).

-spec stop() -> ok.
stop() ->
  server:stop(frequency).

-spec allocate() -> {ok, frequency()} | {error, no_frequency}.
allocate() ->
  server:call(frequency, {allocate, self()}).

-spec deallocate(frequency()) -> ok.
deallocate(Frequency) ->
  server:call(frequency, {deallocate, Frequency}).


% callbacks

-spec init() -> frequencies().
init() ->
  {possible_frequencies(), []}.

-spec terminate(frequencies()) -> ok.
terminate(_Frequencies) ->
  ok.

-spec handle(Message::any(), frequencies()) -> {frequencies(), Reply::any()}.
handle({allocate, From}, Frequencies) ->
  allocate(Frequencies, From);
handle({deallocate, Frequency}, Frequencies) ->
  deallocate(Frequencies, Frequency).


% private

-spec allocate(frequencies(), pid()) -> {frequencies(), {ok, frequency()} | {error, no_frequency}}.
allocate(Frequencies = {[], _Allocated}, _Pid) ->
  {Frequencies, {error, no_frequency}};
allocate({[Frequency|Free], Allocated}, Pid) ->
  {{Free, [{Frequency, Pid} | Allocated]}, {ok, Frequency}}.

-spec deallocate(frequencies(), frequency()) -> {frequencies(), ok}.
deallocate({Free, Allocated}, Frequency) ->
  AllocatedAfterAllocation = lists:keydelete(Frequency, 1, Allocated),
  {{[Frequency|Free], AllocatedAfterAllocation}, ok}.

-spec possible_frequencies() -> [frequency()].
possible_frequencies() ->
  [10, 11, 12, 13, 14, 15].
