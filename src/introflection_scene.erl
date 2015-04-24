-module(introflection_scene).

%% Public exports
-export([start_link/0,
         add_module/2]).

-include("logger.hrl").

-type scene() :: pid().

-export_type([scene/0]).

%% ===================================================================
%% API functions
%% ===================================================================

-spec start_link() -> {ok, Scene} when
      Scene :: scene().

start_link() ->
    {ok, Pid} = gen_event:start_link(),
    ok = gen_event:add_handler(Pid, introflection_module_space, []),
    {ok, Pid}.

-spec add_module(Scene, Module) -> ok when
      Scene :: scene(),
      Module :: introflection_module:rmodule().

add_module(Pid, Module) ->
    gen_event:notify(Pid, {add_module, Module}).
