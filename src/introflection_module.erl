-module(introflection_module).

%% Public exports
-export([add/1,
         find/1,
         annotate/1,
         deannotate/1]).

%% Internal exports
-export([init/1,
         install/1]).

-export_type([rmodule/0]).

-include("tables.hrl").

-record(introflection_modules, {object_id :: object_id(),
                                name      :: name(),
                                nesting   :: nesting(),
                                parent    :: object_id()}).

-type object_id() :: pos_integer().
-type name() :: string().
-type nesting() :: non_neg_integer().
-type rmodule() :: #introflection_modules{}.
-type rmodule_map() :: map().

%% ===================================================================
%% API functions
%% ===================================================================

-spec add(rmodule()) -> ok.

add({ObjectId, Name, Nesting, Parent}) ->
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

-spec annotate(rmodule()) -> rmodule_map().

annotate({ObjectId, Name, Nesting, Parent}) ->
    #{object_id => ObjectId,
      name => Name,
      nesting => Nesting,
      parent => Parent}.

-spec deannotate(rmodule_map()) -> rmodule().

deannotate(ModuleMap) ->
    #{object_id := O, name := N, nesting := G, parent := P} = ModuleMap,
    {O, N, G, P}.

-spec init(Nodes) -> {atomic, ok} when
      Nodes :: node_list().

init(Nodes) ->
    {atomic, ok} = mnesia:create_table(introflection_modules, [
                       {type, set},
                       {attributes, record_info(fields, introflection_modules)},
                       {ram_copies, Nodes}]).

-spec install(Nodes) -> ok when
      Nodes :: node_list().

install(Nodes) ->
    ok = mnesia:create_schema(Nodes),
    application:start(mnesia),
    init(Nodes),
    application:stop(mnesia),
    ok.
