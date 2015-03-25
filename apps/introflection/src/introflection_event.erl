-module(introflection_event).

%% API
-export([init/1, install/1]).
-export([bulk_store/2, store_event/3, modadds/0]).

-include_lib("stdlib/include/qlc.hrl").

-include("mnesia_tables.hrl").
-include("events.hrl").
-include("logger.hrl").

%% ===================================================================
%% API functions
%% ===================================================================

init(Nodes) ->
    {atomic, ok} = mnesia:create_table(introflection_events, [
                       {type, set},
                       {attributes, record_info(fields, introflection_events)},
                       {ram_copies, Nodes}]).

install(Nodes) ->
    ok = mnesia:create_schema(Nodes),
    application:start(mnesia),
    init(Nodes),
    application:stop(mnesia).

bulk_store(_ScenePid, []) ->
    ok;
bulk_store(ScenePid, [Event|Events]) ->
    #{event := Type, data := Data} = Event,
    store_event(ScenePid, Type, Data),
    bulk_store(ScenePid, Events).

store_event(ScenePid, Type, #{object_id := O, name := N, nesting := G, parent := P})
  when Type =:= ?MODULE_ADDED ->
    F = fun () ->
        ok = introflection_module:add(O, N, G, P),
        introflection_scene:add_module(ScenePid, {O, N, G, P}),
        mnesia:write(#introflection_events{type=?MODULE_ADDED, ref=O})
    end,
    {atomic, ResultOfFun} = mnesia:transaction(F),
    ResultOfFun.

modadds() ->
    F = fun() ->
        Query = qlc:q(
            [#{event => T, data => introflection_module:annotate(
                                     introflection_module:find(Ref)
                                    )} ||
                #introflection_events{id=_Id,
                                      type=T,
                                      ref=Ref} <- mnesia:table(introflection_events),
                T =:= ?MODULE_ADDED]),
        qlc:eval(Query)
    end,
    {atomic, ResultOfFun} = mnesia:transaction(F),
    ResultOfFun.
