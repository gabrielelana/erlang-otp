-module(frequency).
-behaviour(gen_server).
-export_type([frequency/0]).
-export([start/0, stop/0, allocate/0, deallocate/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

-type frequency() :: 10..15.
-type allocated_frequency() :: {frequency(), pid()}.
-type allocation_message() :: {allocate, pid()}.
-type deallocation_message() :: {deallocate, frequency()}.
-type stop_message() :: stop.
-type allocation_reply() :: {ok, frequency()} | {error, no_frequency}.
-type deallocation_reply() :: ok.
-type termination_reply() :: ok.
-type frequencies() :: {[frequency()], [allocated_frequency()]}.


%% client API
-spec start() -> ignore | {error, _} | {ok, pid()}.
start() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec stop() -> termination_reply().
stop() ->
  gen_server:cast(?MODULE, stop).

-spec allocate() -> allocation_reply().
allocate() ->
  gen_server:call(?MODULE, {allocate, self()}).

-spec deallocate(frequency()) -> deallocation_reply().
deallocate(Freq) ->
  gen_server:cast(?MODULE, {deallocate, Freq}).


%% callbacks

-spec init(_) -> {ok, frequencies()}.
init(_Args) ->
  {ok, {possible_frequencies(), []}}.

-spec handle_call(allocation_message(), {pid(), _}, frequencies()) ->
  {reply, {error, no_frequency} | {ok, frequency()}, frequencies()}.
handle_call({allocate, Pid}, _From, Frequencies) ->
  {FrequenciesAfterAllocation, Reply} = allocate(Frequencies, Pid),
  {reply, Reply, FrequenciesAfterAllocation}.

-spec handle_cast(deallocation_message() | stop_message(), frequencies()) ->
  {noreply, frequencies()} | {stop, normal, frequencies()}.
handle_cast({deallocate, Freq}, Frequencies) ->
  FrequenciesAfterDeallocation = deallocate(Frequencies, Freq),
  {noreply, FrequenciesAfterDeallocation};
handle_cast(stop, Frequencies) ->
  {stop, normal, Frequencies}.

-spec handle_info(term(), frequencies()) -> {noreply, frequencies()}.
handle_info(_Message, LoopData) ->
  {noreply, LoopData}.

-spec code_change(term(), frequencies(), term()) ->
  {ok, frequencies()} | {error, term()}.
code_change(_OldVsn, Frequencies, _Extra) ->
  {ok, Frequencies}.

-spec terminate(atom(), frequencies()) -> term().
terminate(_Reason, _Frequencies) ->
  ok.


%% private

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
