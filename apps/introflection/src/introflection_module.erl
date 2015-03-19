-module(introflection_module).

-include("mnesia_tables.hrl").
-include("logger.hrl").

%% API
-export([init/1, install/1]).
-export([add/4, find/1]).

%% ===================================================================
%% API functions
%% ===================================================================

init(Nodes) ->
    {atomic, ok} = mnesia:create_table(introflection_modules, [
                       {type, set},
                       {attributes, record_info(fields, introflection_modules)},
                       {ram_copies, Nodes}]).

install(Nodes) ->
    ok = mnesia:create_schema(Nodes),
    application:start(mnesia),
    init(Nodes),
    application:stop(mnesia).

add(ObjectId, Name, Nesting, Parent) ->
    F = fun() ->
        mnesia:write(#introflection_modules{object_id=ObjectId,
                                            name=Name,
                                            nesting=Nesting,
                                            parent=Parent})
    end,
    {atomic, ResultOfFun} = mnesia:transaction(F),
    ResultOfFun.

find(ObjectId) ->
    F = fun() ->
        case mnesia:read({introflection_modules, ObjectId}) of
            [#introflection_modules{name=N, nesting=L, parent=P}] ->
                {ObjectId, N, L, P};
            [] ->
                undefined
        end
    end,
    {atomic, ResultOfFun} = mnesia:transaction(F),
    ResultOfFun.

%% find_chain(ObjectId) ->
