-module(erlq_memcache).

-export([process/3, stop/1]).

-include("erlq.hrl").

stop(State) ->
    {ok, State}.

process(Socket, Data, State) ->
    % io:format("parse data ~p~n", [string:tokens(binary_to_list(Data), " \r\n")]),
    dispatch(Socket, string:tokens(binary_to_list(Data), " \r\n"), State).

dispatch(_Socket, ["get", Key], State) ->
    case erlq_queue_proxy:where(Key) of
        undefined ->
            {ok, Pid} = erlq_app:start_queue(Key),
            erlq_queue_proxy:add(Key, Pid),
            {reply, <<"END\r\n">>, State};
        {ok, Pid} ->
            case erlq_queue:get(Pid, Key) of
                {ok, empty} ->
                    {reply, ["END\r\n"], State};
                {error, _} ->
                    {reply, <<"ERROR\r\n">>, State};
                {_, Data} ->
                    Response = get_response(Key, Data),
                    erlq_stat:incr_cmd_get(Key),
                    erlq_stat:add_bytes_read(Key, Data),
                    {reply, [Response|"END\r\n"], State}
            end;
        _ ->
            send_error_and_close("Failed to read.", State)
    end;

dispatch(_Socket, ["set", _Key, _Flags, "0", _Bytes] = Data, State) ->
    inet:setopts(_Socket, [{packet, raw}, {active, false}]),
    Result = recv_set_data(_Socket, Data, State),
    inet:setopts(_Socket, [{packet, line}, {active, once}]),
    Result;

dispatch(_Socket, ["set", Key, _Flags, "0", _Bytes, Value], State) ->
    % io:format("start set data ~p~n", [Key]),
    NewValue = list_to_binary(Value),
    % io:format("set value is ~p~n", [NewValue]),
    Ref = case erlq_queue_proxy:where(Key) of
              undefined ->
                  {ok, Pid} = erlq_app:start_queue(Key),
                  erlq_queue_proxy:add(Key, Pid),
                  Pid;
              {ok, Pid} ->
                  Pid
          end,
    case erlq_queue:put(Ref, Key, NewValue) of
        ok ->
            gen_tcp:send(_Socket, <<"STORED\r\n">>),
            erlq_stat:incr_cmd_set(Key),
            erlq_stat:add_bytes_write(Key, NewValue),
            {noreply, State};
        _ ->
            send_error_and_close("Failed to write.", State)
    end;

dispatch(_Socket, ["quit"], State) ->
    {close, State};

dispatch(_Socket, ["stats"], State) ->
    Response =
        lists:map(
          fun({Name, Value}) ->
                  ["STAT " ++ atom_to_list(Name) ++ " " ++ Value ++ "\r\n"]
          end,
          erlq_stat:all()
         ),
    {reply, [Response|"END\r\n"], State};

dispatch(_Socket, _Unknown, State) ->
    {reply, <<"ERROR\r\n">>, State}.

get_response(Key, Data) ->
    [io_lib:format("VALUE ~s ~s ~w", [Key, "0", byte_size(Data)]), "\r\n", Data, "\r\n"].

recv_set_data(_Socket, ["set", Key, _Flags, "0", Bytes], State) ->
    % io:format("start recv set data ~p~n", [Key]),
    case gen_tcp:recv(_Socket, list_to_integer(Bytes), ?MEMCACHE_TIMEOUT) of
        {ok, Value} ->
            gen_tcp:recv(_Socket, 2, ?MEMCACHE_TIMEOUT),
            % io:format("set value is ~p~n", [Value]),
            Ref = case erlq_queue_proxy:where(Key) of
                      undefined ->
                          {ok, Pid} = erlq_app:start_queue(Key),
                          erlq_queue_proxy:add(Key, Pid),
                          Pid;
                      {ok, Pid} ->
                          Pid
                  end,
            case erlq_queue:put(Ref, Key, Value) of
                ok ->
                    gen_tcp:send(_Socket, <<"STORED\r\n">>),
                    erlq_stat:incr_cmd_set(Key),
                    erlq_stat:add_bytes_write(Key, Value),
                    {noreply, State};
                _ ->
                    send_error_and_close("Failed to write.", State)
            end;
        _Other ->
            {noreply, State}
    end. 

send_error_and_close(Message, State) ->
    error_logger:warning_msg("send_error_and_close/2: ~p~n", [Message]),
    {close, ["SERVER_ERROR ", Message, "\r\n"], State}.

