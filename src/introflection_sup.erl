-module(introflection_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-include("otp_types.hrl").

-define(SERVER, ?MODULE).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

-spec start_link() -> sup_start_link_reply().

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

-spec init(sup_args()) -> sup_init_reply().

init([]) ->
    {ok, { {one_for_one, 5, 10},
           [?CHILD(introflection_webserver, worker),
            ?CHILD(introflection_tcpserver_sup, supervisor)]} }.
