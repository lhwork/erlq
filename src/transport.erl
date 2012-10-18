
-module(transport).
-behavior(gen_server).

-export([start_link/1, stop/1, set_socket/2]).

-export([init/1, handle_call/3, handle_cast/2, 
         handle_info/2, terminate/2, code_change/3]).

-record(state, {
          port,
          socket,
          transport
         }).

set_socket(Ref, Sock) ->
    gen_server:cast(Ref, {set_socket, Sock}).

start_link(Port) 
  when is_integer(Port) ->
    gen_server:start_link(?MODULE, [Port], []).

stop(Ref) ->
    gen_server:cast(Ref, stop).

init([Port]) ->
    process_flag(trap_exit, true),
    erlq_stat:incr_curr_conns(),
    {ok, #state{port = Port, transport = erlq_flash }}.

handle_cast({set_socket, Socket}, State) ->
    inet:setopts(Socket, [{active, once}, 
                          {packet, line},
                          binary]),    
    {noreply, State#state{socket = Socket}};

handle_cast(stop, State) ->
    {stop, normal, State};

handle_cast(Event, State) ->
    {stop, {unknown, Event}, State}.

handle_call(Event, From, State) ->
    {stop, {unknown, Event, From}, State}.

handle_info({tcp_closed, Socket}, State) 
  when Socket == State#state.socket ->
    {stop, normal, State};

handle_info({tcp, Socket, Data}, State)
  when Socket == State#state.socket ->
    inet:setopts(Socket, [{active, once}]),
    dispatch(Data, erlq_flash, State);

handle_info({'EXIT', _, _}, State) ->
    %% ignore proxy exit
    {noreply, State};

handle_info(Info, State) ->
    {stop, {unknown_info, Info}, State}.

terminate(_Reason, State) ->
    Mod = State#state.transport,
    Mod:stop(State),
    erlq_stat:decr_curr_conns().

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% Brand new transport

%%% Existing connection 

dispatch(Data, Mod, State = #state{transport = Mod, socket = Socket}) ->
    io:format("dispatch data : ~p~n", [Data]),
    case Mod:process(Socket, Data, State) of
        {reply, DataToSend, State} ->
            gen_tcp:send(Socket, DataToSend),
            {noreply, State};
        {noreply, State} ->
            {noreply, State};
        {close, State} ->
            gen_tcp:close(Socket),
            {stop, normal, State};
        {close, DataToSend, State} ->
            gen_tcp:send(Socket, DataToSend),
            gen_tcp:close(Socket),
            {stop, normal, State};
        Other ->
            error_logger:warning_msg("call_mod(~p) ~p", [Mod, {unexpected_result, Other}])
    end.
