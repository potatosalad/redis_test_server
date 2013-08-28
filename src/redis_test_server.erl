%%%-------------------------------------------------------------------
%%% @author Andrew Bennett <andrew@pagodabox.com>
%%% @copyright 2013, Pagoda Box, Inc.
%%% @doc
%%%
%%% @end
%%% Created :   27 Jun 2013 by Andrew Bennett <andrew@pagodabox.com>
%%%-------------------------------------------------------------------
-module(redis_test_server).
-behaviour(gen_server).

-include("redis_test_server.hrl").

%% API
-export([manual_start/0]).
-export([start_link/0, start_listener/1, start_listener/2, stop_listener/1,
         get_uri/1, get_pid/1, get_port/1, reserve_port/2, get_path/1, reserve_path/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-ignore_xref([start_link/0]).

-type ref() :: any().
-export_type([ref/0]).

-define(SERVER, ?MODULE).
-define(TAB, ?MODULE).

-type monitors() :: [{{reference(), pid()}, any()}].
-record(state, {
    monitors = [] :: monitors()
}).

%%%===================================================================
%%% API
%%%===================================================================

manual_start() ->
    application:start(redis_test_server).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

start_listener(Ref) ->
    start_listener(Ref, 0).

start_listener(Ref, Opts) when is_list(Opts) ->
    TCP = proplists:get_value(tcp, Opts, false),
    UNIX = proplists:get_value(unix, Opts, false),
    case {TCP, UNIX} of
        {false, false} ->
            {error, "Options {tcp, true} and/or {unix, true} are required"};
        _ ->
            Config = #config{tcp=TCP, unix=UNIX},
            Config2 = Config#config{
                port = proplists:get_value(port, Opts, Config#config.port),
                prefix = proplists:get_value(prefix, Opts, Config#config.prefix),
                name = proplists:get_value(name, Opts, Config#config.name),
                path = proplists:get_value(path, Opts, Config#config.path)
            },
            manual_start(),
            gen_server:call(?SERVER, {start_listener, Ref, Config2})
    end;
start_listener(Ref, Port) when is_integer(Port) andalso Port >= 0 andalso Port =< 65535 ->
    start_listener(Ref, [{tcp, true}, {port, Port}]).

stop_listener(Ref) ->
    redis_test_server_fsm:stop(get_pid(Ref)).

get_uri(Ref) ->
    Port = get_port(Ref),
    "redis://127.0.0.1:" ++ integer_to_list(Port) ++ "/0".

get_pid(Ref) ->
    ets:lookup_element(?TAB, {pid, Ref}, 2).

get_port(Ref) ->
    ets:lookup_element(?TAB, {port, Ref}, 2).

reserve_port(Ref, Port) ->
    gen_server:call(?SERVER, {reserve_port, Ref, Port}).

get_path(Ref) ->
    ets:lookup_element(?TAB, {path, Ref}, 2).

reserve_path(Ref, Path) ->
    gen_server:call(?SERVER, {reserve_path, Ref, Path}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    Monitors = [{{erlang:monitor(process, Pid), Pid}, Ref} ||
        [Ref, Pid] <- ets:match(?TAB, {{pid, '$1'}, '$2'})],
    {ok, #state{monitors=Monitors}}.

handle_call({start_listener, Ref, Opts}, From, State=#state{monitors=Monitors}) ->
    {ok, Pid} = redis_test_server_fsm_sup:start_fsm(Ref, From, Opts),
    ets:insert(?TAB, {{pid, Ref}, Pid}),
    MonitorRef = erlang:monitor(process, Pid),
    {noreply, State#state{monitors=[{{MonitorRef, Pid}, Ref} | Monitors]}};
handle_call({reserve_port, Ref, Port}, _From, State) ->
    case ets:match(?TAB, {{port, '$1'}, Port}) of
        [] ->
            true = ets:insert(?TAB, {{port, Ref}, Port}),
            {reply, {ok, Port}, State};
        _ ->
            {reply, {error, taken}, State}
    end;
handle_call({reserve_path, Ref, Path}, _From, State) ->
    case ets:match(?TAB, {{path, '$1'}, Path}) of
        [] ->
            true = ets:insert(?TAB, {{path, Ref}, Path}),
            {reply, {ok, Path}, State};
        _ ->
            {reply, {error, taken}, State}
    end;
handle_call(_Request, _From, State) ->
    {reply, ignore, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info({'DOWN', MonitorRef, process, Pid, _}, State=#state{monitors=Monitors}) ->
    {_, Ref} = lists:keyfind({MonitorRef, Pid}, 1, Monitors),
    true = ets:delete(?TAB, {pid, Ref}),
    true = ets:delete(?TAB, {port, Ref}),
    true = ets:delete(?TAB, {path, Ref}),
    Monitors2 = lists:keydelete({MonitorRef, Pid}, 1, Monitors),
    {noreply, State#state{monitors=Monitors2}};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Tests.

-ifdef(TEST).

start_stop_test() ->
    {ok, Pid} = redis_test_server:start_listener(testredis),
    Pid = redis_test_server:get_pid(testredis),
    Port = redis_test_server:get_port(testredis),
    URI = "redis://127.0.0.1:" ++ integer_to_list(Port) ++ "/0",
    URI = redis_test_server:get_uri(testredis),
    ok = redis_test_server:stop_listener(testredis).

start_stop_unix_test() ->
    application:start(redis_test_server),
    {ok, Pid} = redis_test_server:start_listener(unixredis, [{unix, true}, {tcp, true}]),
    Pid = redis_test_server:get_pid(unixredis),
    Port = redis_test_server:get_port(unixredis),
    URI = "redis://127.0.0.1:" ++ integer_to_list(Port) ++ "/0",
    URI = redis_test_server:get_uri(unixredis),
    Path = redis_test_server:get_path(unixredis),
    "/tmp/test-redis." ++ _ = Path,
    ok = redis_test_server:stop_listener(unixredis),
    application:stop(redis_test_server),
    ok.

-endif.
