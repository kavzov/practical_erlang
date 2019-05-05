{
    application,
    mcache,
    [
        {description, "A simple memory cache."},
        {vsn, "0.1"},
        {modules, [mcache_app, mcache_sup, mcache_srv, mcache_storage]},
        {registered, [mcache_srv, mcache_storage]},
        {applications, [kernel, stdlib]},
        {mod, {mcache_app, []} },
        {env, []}
    ]
}.
