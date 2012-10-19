-module(erlq_queue).
-behaviour(gen_server).

-export([start_link/1, put/3, get/2, items/2, memory/1, flush/2, get_proc_name/1]).

-export([
         init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3
        ]).

-define(SERVER, ?MODULE).

start_link(Queue) ->
    gen_server:start_link(?MODULE, {Queue}, []).

get_proc_name(Queue) ->
    list_to_atom("queue_"++Queue).

put(Ref, Queue, Item) ->
    % io:format("queue ~p set ~p is ~p~n", [Ref, Queue, Item]),
    gen_server:call(Ref, {put, Queue, Item}).

get(Ref, Queue) ->
    % io:format("queue ~p get ~p~n", [Ref, Queue]),
    case gen_server:call(Ref, {get, Queue}) of
        {ok, Item} ->
            {ok, Item};
        Other ->
            Other
    end.

items(Ref, Queue) ->
    gen_server:call(Ref, {items, Queue}).

memory(Ref) ->
    gen_server:call(Ref, {memory}).

flush(Ref, Queue) ->
    gen_server:cast(Ref, {new, Queue}).

init({Queue}) ->
    % io:format("~p queue init.~n", [get_proc_name(Queue)]),
    erlang:register(get_proc_name(Queue), self()),
    Dictionary = dict:new(),
    gen_server:cast(self(), {new, Queue}),
    {ok, Dictionary}.

handle_call({get, Queue}, _From, Dictionary) ->
    case dict:find(Queue, Dictionary) of
    {ok, {PidQ, ItemQ}} ->
        % io:format("get ~p ~p~n", [PidQ, ItemQ]),
        case {PidQ, ItemQ} of
        {{_,_},{0,_}} ->
            {reply, {ok, empty}, Dictionary};
        {{0,_},{_,_}} ->
            {NewItemQ, Item} = queue_get_head(ItemQ),
            NewDictionary = dict:store(Queue, {PidQ, NewItemQ}, Dictionary),
            {reply, {ok, Item}, NewDictionary};
        _ ->
            {reply, {error, get_bug}, Dictionary}
        end;
    error ->
        {reply, {error, unknown_ref}, Dictionary}
    end;

handle_call({put, Queue, Item}, _From, Dictionary) ->
    case dict:find(Queue, Dictionary) of
    {ok, {PidQ, ItemQ}} ->
        % io:format("put ~p ~p~n", [PidQ, ItemQ]),
        case {PidQ, ItemQ} of
        {{0,_},{_,_}} ->
            NewItemQ = queue_ins_tail(ItemQ, Item),
            NewDictionary = dict:store(Queue, {PidQ, NewItemQ}, Dictionary),
            {reply, ok, NewDictionary};
        _ ->
            {reply, {error, put_bug}, Dictionary}
        end;
    error ->
        {reply, {error, unknown_ref}, Dictionary}
    end;

handle_call({items, Queue}, _From, Dictionary) ->
    case dict:find(Queue, Dictionary) of
    {ok, {PidQ, ItemQ}} ->
        case {PidQ, ItemQ} of
        {{0,_},{Total,_}} ->
            {reply, {ok, Total}, Dictionary};
        _ ->
            {reply, {error, items_bug}, Dictionary}
        end;
    error ->
        {reply, {error, unknown_ref}, Dictionary}
    end;

handle_call({memory}, _From, Dictionary) ->
    Result = case erlang:process_info(self(), [memory]) of
                [{memory, Size}] -> Size;
                _ -> 0
             end,
    % io:format("~w ~n", [Result]),
    {reply, {ok, Result}, Dictionary};

handle_call(_Request, _From, Dictionary) ->
    {reply, {error, unknown_request}, Dictionary}.

handle_cast({new, Queue}, Dictionary) ->
    PidQ = new_queue(),
    ItemQ = new_queue(),
    NewDictionary = dict:store(Queue, {PidQ, ItemQ}, Dictionary),
    {noreply, NewDictionary};

handle_cast(_Msg, Dictionary) ->
    {noreply, Dictionary}.

handle_info(_Info, Dictionary) ->
    {noreply, Dictionary}.

terminate(_Reason, _Dictionary) ->
    ok.

code_change(_OldVsn, Dictionary, _Extra) ->
    {ok, Dictionary}.

%%% Internal functions
new_queue() ->
    {0, queue:new()}.

queue_ins_tail({Qlen, Queue}, Item) ->
    NewQueue = queue:snoc(Queue, Item),
    {Qlen+1, NewQueue}.

queue_get_head({0, _Queue}) ->
    empty;

queue_get_head({Qlen, Queue}) ->
    Item = queue:head(Queue),
    NewQueue = queue:tail(Queue),
    {{Qlen-1, NewQueue}, Item}. 

