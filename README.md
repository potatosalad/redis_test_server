Erlang Redis Test Server
========================

Launches [redis-server](http://redis.io/) with OS assigned or specific port for use in testing environments.

```erlang
%% OS assigned port
{ok, Pid} = redis_test_server:start_listener(myredis),
Pid = redis_test_server:get_pid(myredis),
Port = redis_test_server:get_port(myredis),
URI = redis_test_server:get_uri(myredis), %% redis://127.0.0.1:12345/0
ok = redis_test_server:stop_listener(myredis).

%% Specified port
{ok, Pid} = redis_test_server:start_listener(myredis, 12345),
ok = redis_test_server:stop_listener(myredis).
```
