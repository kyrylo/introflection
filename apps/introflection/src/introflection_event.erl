-module(introflection_event).

-include_lib("stdlib/include/qlc.hrl").

-include("introflection.hrl").
-include("mnesia_tables.hrl").
-include("events.hrl").
-include("logger.hrl").

%% API
-export([init/1, install/1]).
-export([bulk_store/1, encode/1, add_modadd/1, modadds/0]).

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

bulk_store([]) ->
    ok;
bulk_store([Event|Events]) ->
    #{event := _Type, data := Module} = Event,
    #{object_id := O, name := N, nesting := G, parent := P} = Module,
    ok = introflection_event:add_modadd({O, N, G, P}),
    gproc:send({p, l, {introflection_websocket, ?WSBCAST}},
               {self(), {introflection_websocket, ?WSBCAST},
                introflection_event:encode(Event)}),
    bulk_store(Events).

encode(Data) ->
    jiffy:encode(Data, [force_utf8]).

add_modadd({ObjectId, Name, Nesting, Parent}) ->
    F = fun () ->
        ok = introflection_module:add(ObjectId, Name, Nesting, Parent),
        mnesia:write(#introflection_events{type=?MODULE_ADDED, ref=ObjectId})
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
