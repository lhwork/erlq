
-module(erlq_queue_proxy).
-behavior(gen_server).

-export([start/0, add/2, where/1]).

-export([init/1, handle_call/3, handle_cast/2, 
         handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
          key = ets:new(queue_proxy, [set])
         }).

add(Key, Pid) ->
    gen_server:call(?SERVER, {add, Key, Pid}).

where(Key) ->
    gen_server:call(?SERVER, {where, Key}).

start() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    process_flag(trap_exit, true),
    {ok, #state{}}.

handle_cast(stop, State) ->
    {stop, normal, State};

handle_cast(Event, State) ->
    {stop, {unknown_cast, Event}, State}.

handle_call({where, Key}, _, State) ->
    Result = case ets:lookup(State#state.key, Key) of
                 [{_, Pid}] ->
                     {ok, Pid};
                 _ ->
                     undefined
             end,
    {reply, Result, State};

handle_call({add, Key, Pid}, _, State) ->
    case ets:lookup(State#state.key, Key) of
        [_] ->
            ok;
        _ ->
            ets:insert(State#state.key, {Key, Pid})
    end,
    {reply, ok, State};

handle_call(Event, From, State) ->
    {stop, {unknown_call, Event, From}, State}.

handle_info(Info, State) ->
    {stop, {unknown_info, Info}, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

