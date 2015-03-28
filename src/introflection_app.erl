-module(introflection_app).

-behaviour(application).

%% Application callbacks
-export([start/2,
         stop/1]).

-include("otp_types.hrl").

%% ===================================================================
%% Application callbacks
%% ===================================================================

-spec start(app_start_type(), app_start_args()) -> app_start_reply().

start(normal, []) ->
    introflection_module:init([node()]),
    introflection_event:init([node()]),
    mnesia:wait_for_tables(mnesia:system_info(local_tables), infinity),
    introflection_sup:start_link();
start(_Type, _Args) ->
    introflection_sup:start_link().

-spec stop(app_stop_state()) -> ok.

stop(_State) ->
    ok.
