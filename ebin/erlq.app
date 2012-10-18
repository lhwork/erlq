{application, erlq,
 [{description, "Erlang Queue"},
  {vsn, "0.0.1"},
  {id, "erlq"},
  {modules, [
             erlq,
             erlq_acceptor,
             erlq_admin,
             erlq_app,
             erlq_flash,
             transport,
             erlq_queue,
             erlq_queue_proxy,
             erlq_stat
            ]},
  {registered, [erlq_sup, 
                erlq_transport_sup,
                erlq_queue_sup,
                erlq_queue_proxy,
                erlq_stat]},
  {applications, [kernel, 
                  stdlib 
                 ]},
  {mod, {erlq_app, []}},
  {env, []}
 ]
}.