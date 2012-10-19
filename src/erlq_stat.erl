% 
-module(erlq_stat).
-behaviour(gen_server).

-export([start_link/0, stop/0]).
-export([all/0, incr_curr_conns/0, decr_curr_conns/0, incr_cmd_get/1, incr_cmd_set/1, add_bytes_read/2,
         add_bytes_write/2]).
-export([
         init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3
        ]).

-define(BOOTTIME, {Msec, Sec, _Usec} = now(),1000000 * Msec + Sec.).

-record(stat, {
          boot_time, cmd_get, cmd_set, bytes_read, bytes_write
    }).

-record(state, {curr_conns=0, store=dict:new()}).

-define(SERVER, ?MODULE).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], _Opts = []).

init(_Args) ->
    {ok, #state{}}.

terminate(_Reason, _State) ->
    ok.

init_queue_stat() ->
    {Msec, Sec, _Usec} = now(),
    BootTime = 1000000 * Msec + Sec,
    NewStat = #stat{
          boot_time            = BootTime,
          cmd_get              = 0,
          cmd_set              = 0,
          bytes_read           = 0,
          bytes_write          = 0
        },
    NewStat.


get_stat(Queue, State) ->
    case dict:find(Queue, State#state.store) of
        {ok, Stat} ->
            Stat;
        _ ->
            init_queue_stat()
    end.

global_stat(Name, State) ->
    case Name of
        time ->
            {Msec, Sec, _Usec} = now(),
            Time = 1000000 * Msec + Sec,
            {time, integer_to_list(Time)};
        version ->
            Version =
                case application:get_key(erlq, vsn) of
                    {ok, V} -> V;
                    _       -> "0"
                end,
            {version, Version};
        curr_connections ->
            {curr_connections, integer_to_list(State#state.curr_conns)};
        erlang_procs ->
            ErlangProcs = erlang:system_info(process_count),
            {erlang_procs, integer_to_list(ErlangProcs)};
        erlang_version ->
            {erlang_version, erlang:system_info(version)}
    end.

stat(Name, Queue, State) ->
    Stat = get_stat(Queue, State),
    case Name of
        uptime ->
            {Msec, Sec, _Usec} = now(),
            Uptime = 1000000 * Msec + Sec - Stat#stat.boot_time,
            {list_to_atom("queue_"++Queue++"_uptime"), integer_to_list(Uptime)};
        bytes ->
            Bytes = case erlq_queue_proxy:where(Queue) of
                        {ok, Pid} -> 
                            {ok, Memory} = erlq_queue:memory(Pid),
                            Memory;
                        _ -> 0
                    end,
            {list_to_atom("queue_"++Queue++"_bytes"), integer_to_list(Bytes)};
        curr_items ->
            Items = case erlq_queue_proxy:where(Queue) of
                        {ok, Pid} -> 
                            {ok, Size} = erlq_queue:items(Pid, Queue),
                            Size;
                        _ -> 0
                    end, 
            {list_to_atom("queue_"++Queue++"_curr_items"), integer_to_list(Items)};
        cmd_get ->
            {list_to_atom("queue_"++Queue++"_cmd_get"), integer_to_list(Stat#stat.cmd_get)};
        cmd_set ->
            {list_to_atom("queue_"++Queue++"_cmd_set"), integer_to_list(Stat#stat.cmd_set)};
        bytes_read ->
            {list_to_atom("queue_"++Queue++"_bytes_read"), integer_to_list(Stat#stat.bytes_read)};
        bytes_write ->
            {list_to_atom("queue_"++Queue++"_bytes_write"), integer_to_list(Stat#stat.bytes_write)}
    end.

queue_stat(Queue, State) ->
    lists:map(
            fun(Name) -> stat(Name, Queue, State) end,
                [uptime, bytes,
                curr_items, cmd_get, cmd_set,
                bytes_read, bytes_write]
        ).

all(State) ->
    QueueKeys = dict:fetch_keys(State#state.store),
    % io:format("Queue Keys ~p~n", [QueueKeys]),
    TimeStat = lists:map(
                    fun(Name) -> global_stat(Name, State) end,
                        [time, version, curr_connections]
                ),
    % io:format("all time stat: ~p~n", [TimeStat]),
    ErlangStat = lists:map(
                    fun(Name) -> global_stat(Name, State) end,
                        [erlang_procs, erlang_version]
                ),
    % io:format("all erlang stat: ~p~n", [ErlangStat]),
    Stats = TimeStat ++ lists:flatten([queue_stat(Q, State) || Q <- QueueKeys]) ++ ErlangStat,
    {reply, Stats, State}.

incr_curr_conns(State) ->
    State2 = State#state{curr_conns = State#state.curr_conns + 1},
    {noreply, State2}.

decr_curr_conns(State) ->
    State2 = State#state{curr_conns = State#state.curr_conns - 1},
    {noreply, State2}.

incr_cmd_get(Queue, State) ->
    % io:format("incr_cmd_get : ~p~n", [Queue]),
    Stat = get_stat(Queue, State),
    % io:format("incr_cmd_get get_stat : ~p~n", [Stat]),
    Stat2 = Stat#stat{cmd_get = Stat#stat.cmd_get + 1},
    NewStore = dict:store(Queue, Stat2, State#state.store),
    {noreply, State#state{store=NewStore}}.

incr_cmd_set(Queue, State) ->
    Stat = get_stat(Queue, State),
    Stat2 = Stat#stat{cmd_set = Stat#stat.cmd_set + 1},
    NewStore = dict:store(Queue, Stat2, State#state.store),
    {noreply, State#state{store=NewStore}}.

add_bytes_read(Queue, Value, State) ->
    Stat =get_stat(Queue, State),
    Bytes = byte_size(Value),
    Stat2 = Stat#stat{bytes_read = Stat#stat.bytes_read + Bytes},
    NewStore = dict:store(Queue, Stat2, State#state.store),
    {noreply, State#state{store=NewStore}}.

add_bytes_write(Queue, Value, State) ->
    Stat =get_stat(Queue, State),
    Bytes = byte_size(Value),
    Stat2 = Stat#stat{bytes_write = Stat#stat.bytes_write + Bytes},
    NewStore = dict:store(Queue, Stat2, State#state.store),
    {noreply, State#state{store=NewStore}}.

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};
handle_call(all, _From, State) ->
    all(State).
handle_cast(incr_curr_conns, State) ->
    incr_curr_conns(State);
handle_cast(decr_curr_conns, State) ->
    decr_curr_conns(State);
handle_cast({incr_cmd_get, Queue}, State) ->
    incr_cmd_get(Queue, State);
handle_cast({incr_cmd_set, Queue}, State) ->
    incr_cmd_set(Queue, State);
handle_cast({add_bytes_read, Queue, Value}, State) ->
    add_bytes_read(Queue, Value, State);
handle_cast({add_bytes_write, Queue, Value}, State) ->
    add_bytes_write(Queue, Value, State).
handle_info(_Info, State) ->
    {noreply, State}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

stop() ->
    gen_server:call(?SERVER, stop).
all() ->
    gen_server:call(?SERVER, all).
incr_curr_conns() ->
    gen_server:cast(?SERVER, incr_curr_conns).
decr_curr_conns() ->
    gen_server:cast(?SERVER, decr_curr_conns).
incr_cmd_get(Queue) ->
    gen_server:cast(?SERVER, {incr_cmd_get, Queue}).
incr_cmd_set(Queue) ->
    gen_server:cast(?SERVER, {incr_cmd_set, Queue}).
add_bytes_read(Queue, Value) ->
    gen_server:cast(?SERVER, {add_bytes_read, Queue, Value}).
add_bytes_write(Queue, Value) ->
    gen_server:cast(?SERVER, {add_bytes_write, Queue, Value}).
