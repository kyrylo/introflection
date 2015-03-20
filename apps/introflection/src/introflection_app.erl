-module(introflection_app).

-behaviour(application).

-include("logger.hrl").

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(normal, []) ->
    introflection_module:init([node()]),
    introflection_event:init([node()]),
    mnesia:wait_for_tables(mnesia:system_info(local_tables), infinity),
    introflection_sup:start_link();
start(_Type, _Args) ->
    introflection_sup:start_link().

stop(_State) ->
    ok.
