-module(introflection_event_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([init_per_suite/1, end_per_suite/1,
         init_per_testcase/2, end_per_testcase/2,
         all/0]).

-export([modadds/1]).

all() ->
    [modadds].

%% ===================================================================
%% Callback functions
%% ===================================================================

init_per_suite(Config) ->
    Priv = ?config(priv_dir, Config),
    application:set_env(mnesia, dir, Priv),
    introflection_event:install([node()]),
    application:start(mnesia),
    application:start(introflection_app),
    Config.

end_per_suite(_Config) ->
    application:stop(mnesia),
    ok = mnesia:delete_schema([node()]),
    ok.

init_per_testcase(_, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

%% ===================================================================
%% Test functions
%% ===================================================================

modadds(_Config) ->
    introflection_module:init([node()]),
    Ref = 20035120,
    Ref2 = 19950820,
    ok = introflection_event:store_event({Ref, "Object", 0, 20035120}),
    ok = introflection_event:store_event({Ref2, "Pry", 1, 20035120}),
    {Ref, "Object", 0, 20035120} = introflection_module:find(Ref),
    {Ref2, "Pry", 1, 20035120} = introflection_module:find(Ref2),
    [#{data := #{name := "Object", nesting := 0, object_id := 20035120,
                 parent := 20035120}, event := 0},
     #{data := #{name := "Pry", nesting := 1, object_id := 19950820,
                 parent := 20035120}, event := 0}
    ] = lists:sort(introflection_event:modadds()).
