
-module(erlq_app).
-behaviour(application).

-export([start_transport/1]).
-export([start_queue/1]).
-export([start/2, stop/1, init/1]).

-define(MAX_RESTART,    5).
-define(MAX_TIME,      60).
-define(LISTEN_PORT, 8081).

%% A startup function for new client connection handling.
%% To be called by the TCP listener process.

start_transport(Port) ->
    supervisor:start_child(erlq_transport_sup, [Port]).

start_queue(Key) ->
    supervisor:start_child(erlq_queue_sup, [Key]).

start(_Type, _Args) ->
    Port = erlq_admin:get_env(listen_port, ?LISTEN_PORT),
    supervisor:start_link({local, ?MODULE}, 
                          ?MODULE, 
                          [Port, transport]).

stop(_S) ->
    ok.

%% Supervisor behaviour callbacks

init([Port, Module]) ->
    filelib:ensure_dir("./logs/"),
    LogPath = erlq_admin:get_env(log_path, "./logs/erlq.log"),
    error_logger:add_report_handler(ejabberd_logger_h, LogPath),
    {ok,
     {_SupFlags = {one_for_one, ?MAX_RESTART, ?MAX_TIME},
      [
       %% TCP server
       {erlq_sup,
        {erlq_acceptor, start_link, [self(), Port, Module]},
        permanent,
        2000,
        worker,
        [erlq_acceptor]
       },
       %% Queue proxy
       {erlq_queue_proxy,
        {erlq_queue_proxy, start, []},
        permanent,
        2000,
        worker,
        [erlq_queue_proxy]
        },
       %% Server stats
       {erlq_stat,
        {erlq_stat, start_link, []},
        permanent,
        2000,
        worker,
        [erlq_stat]
       },
       %% Queue
       {erlq_queue_sup,
        {supervisor, start_link, [{local, erlq_queue_sup}, ?MODULE, []]},
        permanent,
        infinity,
        supervisor,
        []
       },       
       %% Client instance supervisor
       {erlq_transport_sup,
        {supervisor, start_link, [{local, erlq_transport_sup}, 
                                  ?MODULE, [Module]]},
        permanent,
        infinity,
        supervisor,
        []
       }
      ]
     }
    };

init([Module]) ->
    {ok,
     {_SupFlags = {simple_one_for_one, ?MAX_RESTART, ?MAX_TIME},
      [
       %% TCP Client
       {undefined,
        {Module, start_link, []},
        temporary,
        2000,
        worker,
        []
       }
      ]
     }
    };

init([]) ->
    {ok,
     {_SupFlags = {simple_one_for_one, ?MAX_RESTART, ?MAX_TIME},
      [
       %% Queue
       {undefined,
        {erlq_queue, start_link, []},
        temporary,
        2000,
        worker,
        []
       }
      ]
     }
    }.



