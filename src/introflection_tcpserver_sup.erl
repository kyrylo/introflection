-module(introflection_tcpserver_sup).

-behaviour(supervisor).

-include("logger.hrl").

%% API
-export([start_link/0, start_socket/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).
-define(TCP_PORT, 7331).
-define(TCP_OPTIONS, [binary, {packet, 0},
                      {reuseaddr, true},
                      {active, once}]).

%%%===================================================================
%%% API functions
%%%===================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init([]) ->
    case gen_tcp:listen(?TCP_PORT, ?TCP_OPTIONS) of
        {ok, ListenSocket} ->
            ?INFO("Socket ~p has started listening on port ~p",
                  [ListenSocket, ?TCP_PORT]),
            spawn_link(fun empty_listeners/0),
            {ok, {{simple_one_for_one, 60, 3600},
                  [{socket,
                    {introflection_tcpserver, start_link, [ListenSocket]},
                    temporary, 1000, worker, [introflection_tcpserver]}
                  ]}};
        {error, Reason} ->
            {error, Reason}
    end.

start_socket() ->
    supervisor:start_child(?MODULE, []).

%%%===================================================================
%%% Internal functions
%%%===================================================================

empty_listeners() ->
    [start_socket() || _ <- lists:seq(1, 1)],
    ok.
