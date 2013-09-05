Erlang Redis Test Server
========================

[![Build Status](https://travis-ci.org/potatosalad/redis_test_server.png?branch=master)](https://travis-ci.org/potatosalad/redis_test_server)

Launches [redis-server](http://redis.io/) with OS assigned or specific port or unix socket for use in testing environments.

*Note:* All examples were started with:

```bash
erl -pa ebin -run redis_test_server manual_start
```

## TCP

### OS assigned port

```erl
1> {ok, Pid} = redis_test_server:start_listener(tcpredis).
{ok,<0.40.0>}
2> Pid = redis_test_server:get_pid(tcpredis).
<0.40.0>
3> Port = redis_test_server:get_port(tcpredis).
63781
4> URI = redis_test_server:get_uri(tcpredis).
"redis://127.0.0.1:63781/0"
5> ok = redis_test_server:stop_listener(tcpredis).
ok
```

### Assigned port

```erlang
1> {ok, Pid} = redis_test_server:start_listener(tcpredis, 12345).
{ok,<0.40.0>}
2> Pid = redis_test_server:get_pid(tcpredis).
<0.40.0>
3> Port = redis_test_server:get_port(tcpredis).
12345
4> URI = redis_test_server:get_uri(tcpredis).
"redis://127.0.0.1:12345/0"
5> ok = redis_test_server:stop_listener(tcpredis).
ok
```

## UNIX Socket

### Random path

```erl
1> {ok, Pid} = redis_test_server:start_listener(unixredis, [{unix, true}]).
{ok,<0.40.0>}
2> Pid = redis_test_server:get_pid(unixredis).
<0.40.0>
3> Path = redis_test_server:get_path(unixredis).
"/tmp/test-redis.50001849.sock"
4> ok = redis_test_server:stop_listener(unixredis).
ok
```

### Custom prefix and name for random path

```erl
1> {ok, Pid} = redis_test_server:start_listener(unixredis, [{unix, true}, {prefix, "/custom/path"}, {name, "custom-name"}]).
{ok,<0.40.0>}
2> Pid = redis_test_server:get_pid(unixredis).
<0.40.0>
3> Path = redis_test_server:get_path(unixredis).
"/custom/path/custom-name.50001849.sock"
4> ok = redis_test_server:stop_listener(unixredis).
ok
```

### Assigned path

```erl
1> {ok, Pid} = redis_test_server:start_listener(unixredis, [{unix, true}, {path, "/tmp/test-redis.sock"}]).
{ok,<0.40.0>}
2> Pid = redis_test_server:get_pid(unixredis).
<0.40.0>
3> Path = redis_test_server:get_path(unixredis).
"/tmp/test-redis.sock"
4> ok = redis_test_server:stop_listener(unixredis).
ok
```

## TCP and UNIX Socket combined

```erl
1> {ok, Pid} = redis_test_server:start_listener(comboredis, [{unix, true}, {path, "/tmp/test-redis.sock"}, {tcp, true}, {port, 12345}]).
{ok,<0.40.0>}
2> Pid = redis_test_server:get_pid(comboredis).
<0.40.0>
3> Path = redis_test_server:get_path(comboredis).
"/tmp/test-redis.sock"
4> Port = redis_test_server:get_port(comboredis).
12345
5> URI = redis_test_server:get_uri(comboredis).
"redis://127.0.0.1:12345/0"
6> ok = redis_test_server:stop_listener(comboredis).
ok
```
