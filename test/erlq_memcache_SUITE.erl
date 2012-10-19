-module(erlq_memcache_SUITE).
-compile(export_all).

all() -> [no_command_test_case, command_test_case, stats_test_case]. 

init_per_testcase(_TestCase, Config) ->
    erlq:start(),
    Config.

end_per_testcase(_TestCase, _Config) ->
    erlq:stop().    

command_test_case(_Conf) ->
    Socket = connect(),

    Value = <<"value1">>,
    Buf = io_lib:format("set key1 0 0 ~w\r\n~s\r\n", [byte_size(Value), Value]),
    gen_tcp:send(Socket, Buf),

    {ok, <<"STORED\r\n">>} = gen_tcp:recv(Socket, 0),

    gen_tcp:send(Socket, "get key1\r\n"),

    {ok, <<"VALUE key1 0 6\r\n">>} = gen_tcp:recv(Socket, 0),
    inet:setopts(Socket, [{packet, raw}]),
    {ok, <<"value1\r\n">>} = gen_tcp:recv(Socket, 6+2),
    inet:setopts(Socket, [{packet, line}]),
    {ok, <<"END\r\n">>} = gen_tcp:recv(Socket, 0),

    gen_tcp:send(Socket, "get key1\r\n"),
    {ok, <<"END\r\n">>} = gen_tcp:recv(Socket, 0),

    close(Socket).

no_command_test_case(_Conf) ->
    Socket = connect(),

    gen_tcp:send(Socket, "no_command\r\n"),
    {ok, <<"ERROR\r\n">>} = gen_tcp:recv(Socket, 0),

    close(Socket).

stats_test_case(_Conf) ->
    Socket = connect(),

    gen_tcp:send(Socket, "stats\r\n"),
    Stats = recv_stats(Socket, []),

    close(Socket).

recv_stats(Socket, Stats) ->
    {ok, Data} = gen_tcp:recv(Socket, 0),
    case Data of
        <<"STAT", _/binary>> ->
            ["STAT", Name|Values] =
                string:tokens(binary_to_list(Data), " \r\n"),
            Stats2 = [{list_to_atom(Name), string:join(Values, " ")} | Stats],
            recv_stats(Socket, Stats2);
        <<"END", _/binary>> ->
            Stats
    end.

connect() ->
    {ok, Socket} =
        gen_tcp:connect({127,0,0,1}, 11211, [binary, {packet, line}, {active, false}]),
    Socket.

close(Socket) ->
    gen_tcp:send(Socket, "quit\r\n"),
    gen_tcp:close(Socket).
