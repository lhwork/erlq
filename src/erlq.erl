
-module(erlq).

-export([start/0, start/1, stop/0]).

start() ->
    start([]).

start([]) ->
    start(11211);

start([Port]) 
  when is_atom(Port) ->
    start(list_to_integer(atom_to_list(Port)));

start(Port)
  when is_integer(Port) ->
    application:set_env(erlq, listen_port, Port),
    application:start(erlq).

stop() ->
    application:stop(erlq).


