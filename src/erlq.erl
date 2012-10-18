
-module(erlq).

-export([start/0, start/1]).

start() ->
    start([]).

start([]) ->
    start(8081);

start([Port]) 
  when is_atom(Port) ->
    start(list_to_integer(atom_to_list(Port)));

start(Port)
  when is_integer(Port) ->
    application:set_env(erlq, listen_port, Port),
    application:start(erlq).



