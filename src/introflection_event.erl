-module(introflection_event).

%% Public exports
-export([bulk_store/2,
         store_event/3,
         modadds/0]).

%% Internal exports.
-export([init/1,
         install/1]).

-export_type([event/0]).

-include_lib("stdlib/include/qlc.hrl").

-include("tables.hrl").
-include("events.hrl").
-include("logger.hrl").

-record(introflection_events, {id={now(), node()},
                               type :: event_type(),
                               ref  :: event_ref()}).

-type event_type() :: non_neg_integer().
-type event_ref() :: non_neg_integer().
-type event() :: map().

%% ===================================================================
%% API functions
%% ===================================================================

-spec bulk_store(Scene, Events) -> ok when
      Scene :: introflection_scene:scene(),
      Events :: [event()].

bulk_store(_ScenePid, []) ->
    ok;
bulk_store(ScenePid, [Event|Events]) ->
    #{event := Type, data := Data} = Event,
    store_event(ScenePid, Type, Data),
    bulk_store(ScenePid, Events).

-spec store_event(Scene, EventType, ModuleMap) -> Result when
      Scene :: introflection_scene:scene(),
      EventType :: event_type(),
      ModuleMap :: introflection_module:rmodule_map(),
      Result :: any().

store_event(ScenePid, Type, ModuleMap) when Type =:= ?MODULE_ADDED ->
    F = fun() ->
        Module = introflection_module:deannotate(ModuleMap),
        {ObjectId,_,_,_} = Module,
        ok = introflection_module:add(Module),
        introflection_scene:add_module(ScenePid, Module),
        mnesia:write(#introflection_events{type=?MODULE_ADDED, ref=ObjectId})
    end,
    {atomic, ResultOfFun} = mnesia:transaction(F),
    ResultOfFun.

-spec modadds() -> Result when
      Result :: any().

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

-spec init(Nodes) -> {atomic, ok} when
      Nodes :: node_list().

init(Nodes) ->
    {atomic, ok} = mnesia:create_table(introflection_events, [
                       {type, set},
                       {attributes, record_info(fields, introflection_events)},
                       {ram_copies, Nodes}]).

-spec install(Nodes) -> ok when
      Nodes :: node_list().

install(Nodes) ->
    ok = mnesia:create_schema(Nodes),
    application:start(mnesia),
    init(Nodes),
    application:stop(mnesia).
