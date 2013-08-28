-record(config, {
    tcp  = false :: boolean(),
    port = 0     :: integer(),

    unix   = false        :: boolean(),
    prefix = "/tmp"       :: string(),
    name   = "test-redis" :: string(),
    path   = undefined    :: undefined | string()
}).
