-define(VERSION, element(2, application:get_key(erlq,vsn))).

-define(MAX_RESTART,    5).
-define(MAX_TIME,      60).
-define(LISTEN_PORT, 8081).

-define(MEMCACHE_TIMEOUT, 5000).

%% Print in standard output

-define(PRINT(Format, Args), io:format(Format, Args)).

%% Logging

-define(DEBUG(Format, Args),
    ejabberd_logger:debug_msg(?MODULE,?LINE,Format, Args)).

-define(INFO_MSG(Format, Args),
    ejabberd_logger:info_msg(?MODULE,?LINE,Format, Args)).
                  
-define(WARNING_MSG(Format, Args),
    ejabberd_logger:warning_msg(?MODULE,?LINE,Format, Args)).
                  
-define(ERROR_MSG(Format, Args),
    ejabberd_logger:error_msg(?MODULE,?LINE,Format, Args)).

-define(CRITICAL_MSG(Format, Args),
    ejabberd_logger:critical_msg(?MODULE,?LINE,Format, Args)).