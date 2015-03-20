-module(introflection_module_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([init_per_suite/1, end_per_suite/1,
         init_per_testcase/2, end_per_testcase/2,
         all/0]).

-export([add/1, find/1, annotate/1]).

all() ->
    [add, find, annotate].

%% ===================================================================
%% Callback functions
%% ===================================================================

init_per_suite(Config) ->
    Priv = ?config(priv_dir, Config),
    application:set_env(mnesia, dir, Priv),
    introflection_module:install([node()]),
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

add(_Config) ->
    ok = introflection_module:add(20035120, "Object", 0, 20035120),
    ok = introflection_module:add(19950820, "Pry", 1, 20035120),
    ok = introflection_module:add(21866280, "Code", 2, 19950820).

find(_Config) ->
    ok = introflection_module:add(20035160, "BasicObject", 0, 20035120),
    {20035160, "BasicObject", 0, 20035120} = introflection_module:find(20035160),
    undefined = introflection_module:find("BasicObject"),
    undefined = introflection_module:find(make_ref()).

annotate(_Config) ->
    #{object_id := 123,
      name := "Tesla",
      nesting := 22,
      parent := 321} = introflection_module:annotate({123, "Tesla", 22, 321}),
   nil = introflection_module:annotate(123).
