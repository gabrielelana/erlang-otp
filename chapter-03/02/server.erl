-module(server).
-export([start/2, stop/1, init/2, call/2]).


start(Mod, Args) ->
  register(Mod, spawn(server, init, [Mod, Args])).

init(Mod, Args) ->
  LoopData = erlang:apply(Mod, init, Args),
  loop(Mod, LoopData).

stop(Name) ->
  Name ! {stop, self()},
  receive {reply, Reply} -> Reply end.

call(Name, Message) ->
  Name ! {request, self(), Message},
  receive {reply, Reply} -> Reply end.

reply(To, Reply) ->
  To ! {reply, Reply}.

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
