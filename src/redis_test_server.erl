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

%% API
-export([start/0, start_link/0, start_listener/1, start_listener/2,
         stop_listener/1, get_uri/1, get_pid/1, get_port/1, reserve_port/2]).

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

start() ->
    application:start(redis_test_server).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

start_listener(Ref) ->
    start_listener(Ref, 0).

start_listener(Ref, Port) ->
    start(),
    gen_server:call(?SERVER, {start_listener, Ref, Port}).

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

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    Monitors = [{{erlang:monitor(process, Pid), Pid}, Ref} ||
        [Ref, Pid] <- ets:match(?TAB, {{pid, '$1'}, '$2'})],
    {ok, #state{monitors=Monitors}}.

handle_call({start_listener, Ref, Port}, From, State=#state{monitors=Monitors}) ->
    {ok, Pid} = redis_test_server_fsm_sup:start_fsm(Ref, From, Port),
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
handle_call(_Request, _From, State) ->
    {reply, ignore, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info({'DOWN', MonitorRef, process, Pid, _}, State=#state{monitors=Monitors}) ->
    {_, Ref} = lists:keyfind({MonitorRef, Pid}, 1, Monitors),
    true = ets:delete(?TAB, {pid, Ref}),
    true = ets:delete(?TAB, {port, Ref}),
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

-endif.
