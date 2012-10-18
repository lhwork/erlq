
-module(erlq_admin).

-export([make_boot/0,
         get_env/1, get_env/2,
         cluster/0, join/1]).

make_boot() ->
    systools:make_script("erlq", [local, {outdir, "./ebin"}]).

%%% Read application configuration variables

get_env(Var) ->
    get_env(Var, undefined).

get_env(Var, Default) ->
    case application:get_env(Var) of
        {ok, Val} -> Val;
        _ -> case init:get_argument(Var) of
                 {ok, [[Val]]} -> Val;
                 {ok, [Val]}   -> Val;
                 _   -> Default
            end
    end.

%%% Nodes to ping to join the cluster

cluster() ->
    Existing = case application:get_env(erlq, cluster) of
                   undefined ->
                       [];
                   {ok, L = [_|_]} ->
                       L;
                   {ok, Name} ->
                       [Name]
               end,
    cluster(Existing, []).

cluster([], Acc) ->
    Acc;

cluster([H|T], Acc) ->
    H1 = if
             is_atom(H) ->
                 atom_to_binary(H, latin1);
             true ->
                 H
         end,
    Node = case re:run(H1, <<"@">>) of
               nomatch ->
                   node_name(H1);
               _ ->
                   H
           end,
    cluster(T, [Node|Acc]).
    

%%% Ping and return unreachable nodes

join(Nodes) ->
    F = fun(Node) ->
                pong == net_adm:ping(Node)
        end,
    lists:filter(F, Nodes).

node_name(Host) 
  when is_binary(Host) ->
    binary_to_atom(list_to_binary(["erlq@", Host]), latin1).
    
