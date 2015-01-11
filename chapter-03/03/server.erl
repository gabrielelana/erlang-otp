-module(server).
-export([start/2, stop/1, init/2, call/2, call/3]).


start(Mod, Args) ->
  register(Mod, spawn(server, init, [Mod, Args])).

init(Mod, Args) ->
  LoopData = erlang:apply(Mod, init, Args),
  loop(Mod, LoopData).

stop(Name) ->
  Ref = make_ref(),
  Name ! {stop, {Ref, self()}},
  receive {reply, Ref, Reply} -> Reply end.

call(Name, Message) ->
  call(Name, Message, 5000).

call(Name, Message, Timeout) ->
  Ref = erlang:monitor(process, Name),
  Name ! {request, {Ref, self()}, Message},
  receive
    {reply, Ref, Reply} ->
      erlang:demonitor(Ref, [flush]),
      Reply;
    {'DOWN', Ref, process, _, Reason} ->
      exit(Reason)
  after Timeout ->
    erlang:demonitor(Ref, [flush]),
    exit(timeout)
  end.

reply({Ref, To}, Reply) ->
  To ! {reply, Ref, Reply}.

loop(Mod, LoopData) ->
  receive
    {request, From, Message} ->
      {NextLoopData, Reply} = Mod:handle(Message, LoopData),
      reply(From, Reply),
      loop(Mod, NextLoopData);
    {stop, From} ->
      Mod:terminate(LoopData),
      reply(From, ok)
  end.
