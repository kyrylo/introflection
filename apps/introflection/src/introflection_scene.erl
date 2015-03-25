-module(introflection_scene).

-export([start_link/0, add_module/2]).

-include("logger.hrl").

start_link() ->
    {ok, Pid} = gen_event:start_link(),
    gen_event:add_handler(Pid, introflection_module_space, []),
    {ok, Pid}.

add_module(Pid, {_ObjectId, _Name, _Nesting, _Parent}=Module) ->
    gen_event:notify(Pid, {add_module, Module}).
