%%%-------------------------------------------------------------------
%%% @author Andrew Bennett <andrew@pagodabox.com>
%%% @copyright 2013, Pagoda Box, Inc.
%%% @doc
%%%
%%% @end
%%% Created :   27 Jun 2013 by Andrew Bennett <andrew@pagodabox.com>
%%%-------------------------------------------------------------------
-module(redis_test_server_fsm_sup).
-behaviour(supervisor).

-include("redis_test_server.hrl").

%% API
-export([start_link/0, start_fsm/3]).

%% Supervisor callbacks
-export([init/1]).

-define(SUPERVISOR, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

start_link() ->
    supervisor:start_link({local, ?SUPERVISOR}, ?MODULE, []).

start_fsm(Ref, From, Config=#config{}) ->
    supervisor:start_child(?SUPERVISOR, [Ref, From, Config]).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init([]) ->
    FSM = {undefined,
        {redis_test_server_fsm, start_link, []},
        temporary, infinity, worker, [redis_test_server_fsm]},
    {ok, {{simple_one_for_one, 5, 10}, [FSM]}}.
