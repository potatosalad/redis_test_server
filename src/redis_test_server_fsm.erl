%%%-------------------------------------------------------------------
%%% @author Andrew Bennett <andrew@pagodabox.com>
%%% @copyright 2013, Pagoda Box, Inc.
%%% @doc
%%%
%%% @end
%%% Created :   27 Jun 2013 by Andrew Bennett <andrew@pagodabox.com>
%%%-------------------------------------------------------------------
-module(redis_test_server_fsm).
-behaviour(gen_fsm).

-include("redis_test_server.hrl").

%% API
-export([start_link/3, stop/1]).

%% gen_fsm callbacks
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3,
         terminate/3, code_change/4]).

-export([get_path/2, get_path/3, get_port/2, get_port/3,
         start_redis/2, start_redis/3, ready/2, ready/3]).

-ignore_xref([start_link/0]).

-define(TIMEOUT, 3000).

-record(state, {
    ref    = undefined   :: undefined | redis_test_server:ref(),
    from   = undefined   :: undefined | pid() | {pid(), any()},
    config = undefined   :: #config{},
    path   = undefined   :: undefined | string(),
    port   = undefined   :: undefined | integer(),
    queue  = queue:new() :: queue(),
    redis  = undefined   :: undefined | port()
}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Ref, From, Config=#config{}) ->
    gen_fsm:start_link(?MODULE, [Ref, From, Config], []).

stop(Pid) ->
    gen_fsm:send_all_state_event(Pid, stop).

%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================

init([Ref, From, Config=#config{port=Port, path=Path}]) ->
    timer:send_after(?TIMEOUT, ready_timeout),
    State = #state{ref=Ref, from=From, config=Config, port=Port, path=Path},
    case {Config#config.tcp, Config#config.unix} of
        {false, false} ->
            {stop, "Options {tcp, true} and/or {unix, true} are required"};
        {_, true} ->
            {ok, get_path, State, 0};
        {true, false} ->
            {ok, get_port, State, 0}
    end.

handle_event(stop, _StateName, State) ->
    {stop, normal, State};
handle_event(_Event, StateName, State) ->
    {next_state, StateName, State, 0}.

handle_sync_event(_Event, _From, StateName, State) ->
    {reply, ok, StateName, State, 0}.

handle_info(ready_timeout, StateName, State=#state{from=undefined}) when StateName =/= ready ->
    {stop, normal, State};
handle_info(ready_timeout, StateName, State=#state{from=From}) when StateName =/= ready ->
    gen_server:reply(From, {error, {timeout, ?TIMEOUT}}),
    {stop, normal, State};
handle_info({Redis, {data, {eol, Data}}}, start_redis, State=#state{redis=Redis, from=From}) ->
    case binary:match(Data, <<"now ready to accept connections">>) of
        Match when Match =:= []; Match =:= nomatch ->
            {next_state, start_redis, State};
        _ ->
            gen_server:reply(From, {ok, self()}),
            {next_state, ready, State#state{from=undefined}, 0}
    end;
handle_info({Redis, ExitStatus={exit_status, _Status}}, _StateName, State=#state{redis=Redis}) ->
    handle_exit_status(ExitStatus, State);
handle_info(_Info, StateName, State) ->
    {next_state, StateName, State, 0}.

get_path(_Event, State=#state{config=Config, path=undefined, ref=Ref}) ->
    Name = io_lib:format("~s.~w.sock", [Config#config.name, erlang:phash2(erlang:make_ref())]),
    Path = filename:join(Config#config.prefix, Name),
    case redis_test_server:reserve_path(Ref, Path) of
        {ok, Path} ->
            State2 = State#state{path=Path},
            case Config#config.tcp of
                true ->
                    {next_state, get_port, State2, 0};
                false ->
                    {next_state, start_redis, State2, 0}
            end;
        {error, taken} ->
            {next_state, get_path, State, 0}
    end;
get_path(_Event, State=#state{config=Config, path=Path, from=From, ref=Ref}) ->
    case redis_test_server:reserve_path(Ref, Path) of
        {ok, Path} ->
            State2 = State#state{path=Path},
            case Config#config.tcp of
                true ->
                    {next_state, get_port, State2, 0};
                false ->
                    {next_state, start_redis, State2, 0}
            end;
        {error, taken} ->
            Error = {error, {path_taken, Path}},
            case From of
                undefined ->
                    ok;
                _ ->
                    gen_server:reply(From, Error)
            end,
            {stop, normal, State}
    end.

get_path(Event, From, State=#state{queue=Queue}) ->
    {next_state, get_path, State#state{queue=queue:in({From, Event}, Queue)}, 0}.

get_port(_Event, State=#state{port=DynamicPort, ref=Ref}) when DynamicPort =:= 0 orelse DynamicPort =:= undefined ->
    {ok, Socket} = gen_tcp:listen(0,
        [binary, {active, false}, {packet, raw}, {reuseaddr, true}, {nodelay, true}]),
    {ok, {_, Port}} = inet:sockname(Socket),
    case redis_test_server:reserve_port(Ref, Port) of
        {ok, Port} ->
            gen_tcp:close(Socket),
            {next_state, start_redis, State#state{port=Port}, 0};
        {error, taken} ->
            gen_tcp:close(Socket),
            {next_state, get_port, State, 0}
    end;
get_port(_Event, State=#state{port=Port, ref=Ref, from=From}) ->
    case redis_test_server:reserve_port(Ref, Port) of
        {ok, Port} ->
            {next_state, start_redis, State, 0};
        {error, taken} ->
            Error = {error, {port_taken, Port}},
            case From of
                undefined ->
                    ok;
                _ ->
                    gen_server:reply(From, Error)
            end,
            {stop, normal, State}
    end.

get_port(Event, From, State=#state{queue=Queue}) ->
    {next_state, get_port, State#state{queue=queue:in({From, Event}, Queue)}, 0}.

start_redis(_Event, State=#state{path=Path, port=Port, config=Config, redis=undefined}) ->
    TCPArgs = case Config#config.tcp of
        true ->
            io_lib:format("--port ~w", [Port]);
        false ->
            "--port 0"
    end,
    UNIXArgs = case Config#config.unix of
        true ->
            io_lib:format("--unixsocket \"~s\"", [re:replace(Path, [$"], "\\\\\"", [global, {return, list}])]);
        false ->
            ""
    end,
    Command = lists:flatten(io_lib:format("~s ~s ~s", [server_script(), TCPArgs, UNIXArgs])),
    PortOpts = [exit_status, use_stdio, binary, {line, 1000}],
    Redis = erlang:open_port({spawn, Command}, PortOpts),
    {next_state, start_redis, State#state{redis=Redis}};
start_redis(_Event, State) ->
    {next_state, start_redis, State}.

start_redis(Event, From, State=#state{queue=Queue}) ->
    {next_state, start_redis, State#state{queue=queue:in({From, Event}, Queue)}, 0}.

ready(_Event, State=#state{queue=[{},{}]}) ->
    {next_state, ready, State};
ready(_Event, State=#state{queue=Queue}) ->
    case queue:out(Queue) of
        {{value, {From, Event}}, Queue2} ->
            Self = self(),
            spawn(fun() ->
                Reply = gen_fsm:sync_send_event(Self, Event),
                gen_server:reply(From, Reply)
            end),
            {next_state, ready, State#state{queue=Queue2}, 0};
        {empty, Queue} ->
            {next_state, ready, State}
    end.

ready(_Event, _From, State) ->
    {reply, ok, ready, State, 0}.

terminate(_Reason, _StateName, #state{redis=undefined}) ->
    ok;
terminate(_Reason, _StateName, #state{redis=Redis}) ->
    case catch erlang:port_close(Redis) of _ -> ok end,
    ok.

code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

handle_exit_status({exit_status, _Status}, State=#state{from=undefined}) ->
    {stop, normal, State};
handle_exit_status({exit_status, Status}, State=#state{from=From}) ->
    gen_server:reply(From, {error, {exit_status, Status}}),
    {stop, normal, State#state{from=undefined}}.

server_script() ->
    PrivDir = case code:priv_dir(redis_test_server) of
        {error, bad_name} ->
            {ok, CWD} = file:get_cwd(),
            filename:join(CWD, "priv");
        Dir ->
            Dir
    end,
    filename:join(PrivDir, "redis-test-server.sh").
