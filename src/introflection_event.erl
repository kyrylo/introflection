-module(introflection_event).

%% Public exports
-export([process/2,
         chains/0,
         emit/2]).

%% Internal exports
-export([init/1,
         install/1]).

%% Debugging exports
-export([modadds/0]).

-export_type([event/0]).

-include_lib("stdlib/include/qlc.hrl").

-include("tables.hrl").
-include("events.hrl").
-include("logger.hrl").

-record(introflection_events, {id={now(), node()},
                               type :: event_type(),
                               direction :: incoming | outgoing,
                               ref  :: event_ref()}).

-type event_type() :: non_neg_integer().
-type event_ref() :: non_neg_integer() | [any()].
-type event() :: map().

%% ===================================================================
%% API functions
%% ===================================================================

-spec process(Scene, Events) -> ok when
      Scene :: introflection_scene:scene(),
      Events :: [event()].

process(_Scene, []) ->
    ok;
process(Scene, [Event|Events]) ->
    #{event := EventType, data := EventData} = Event,
    add_event(Scene, EventType, EventData),
    process(Scene, Events).

-spec chains() -> Result when
      Result :: [[any()]].

chains() ->
    F = fun() ->
        Chains = qlc:eval(qlc:q(
            [Chain || #introflection_events{id=_Id,type=EventType,ref=Chain} <-
                          mnesia:table(introflection_events),
                      EventType =:= ?MODULE_CHAIN_ADDED])),
        [#{event => ?MODULE_CHAIN_ADDED,
           data => lists:map(fun(I) ->
                       Module = introflection_module:find(I),
                       introflection_module:annotate(Module)
                    end, Chain)} || Chain <- Chains]
    end,
    {atomic, ResultOfFun} = mnesia:transaction(F),
    ResultOfFun.

-spec emit(EventType, Data) -> no_return() when
      EventType :: event_type(),
      Data :: any().

emit(EventType, Data) when EventType =:= ?MODULE_ADDED ->
    Event = #{event => EventType, data => map_event_data(Data)},
    introflection_websocket:broadcast(Event);
emit(EventType, Data) when EventType =:= ?MODULE_CHAIN_ADDED ->
    EventList = [map_event_data(Event) || Event <- Data],
    add_event_chain(EventType, EventList),
    Event = #{event => EventType, data => EventList},
    introflection_websocket:broadcast(Event).


%% ===================================================================
%% Private functions
%% ===================================================================

-spec add_event(Scene, EventType, EventData) -> Result when
      Scene :: introflection_scene:scene() | undefined,
      EventType :: event_type(),
      EventData :: any(),
      Result :: any().

add_event(Scene, ?MODULE_ADDED, EventData) ->
    F = fun() ->
        Module = introflection_module:deannotate(EventData),
        {ObjectId,_,_,_} = Module,
        ok = introflection_module:add(Module),
        introflection_scene:add_module(Scene, Module),
        mnesia:write(#introflection_events{type=?MODULE_ADDED,
                                           direction=incoming,
                                           ref=ObjectId})
    end,
    {atomic, ResultOfFun} = mnesia:transaction(F),
    ResultOfFun;
add_event(undefined, ?MODULE_CHAIN_ADDED, Modules) ->
    F = fun() ->
        Chain = [maps:get(object_id, Map) || Map <- Modules],
        mnesia:write(#introflection_events{type=?MODULE_CHAIN_ADDED,
                                           direction=outgoing,
                                           ref=Chain})
    end,
    {atomic, ResultOfFun} = mnesia:transaction(F),
    ResultOfFun.

-spec add_event_chain(EventType, EventData) -> Result when
      EventType :: event_type(),
      EventData :: any(),
      Result :: no_return().

add_event_chain(EventType, EventData) ->
    add_event(undefined, EventType, EventData).

-spec map_event_data(Data) -> AnnotatedData when
      Data :: introflection_module:rmodule() | map(),
      AnnotatedData :: map() | Data.

map_event_data(Data) ->
    case introflection_module:is_annotated(Data) of
        false -> introflection_module:annotate(Data);
        true -> Data
    end.

%% ===================================================================
%% Internal functions
%% ===================================================================

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

%% ===================================================================
%% Debugging functions
%% ===================================================================

-spec modadds() -> Result when
      Result :: any().

modadds() ->
    F = fun() ->
        qlc:eval(qlc:q(
            [#{event => EventType,
               data => introflection_module:annotate(
                         introflection_module:find(Ref))} ||
                #introflection_events{id=_Id,
                                      type=EventType,
                                      ref=Ref} <-
                    mnesia:table(introflection_events),
                EventType =:= ?MODULE_ADDED]))
    end,
    {atomic, ResultOfFun} = mnesia:transaction(F),
    ResultOfFun.
