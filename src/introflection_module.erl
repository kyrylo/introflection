-module(introflection_module).

%% Public exports
-export([add/1,
         find/1,
         annotate/1,
         deannotate/1,
         top_level/0,
         get_object_id/1,
         is_annotated/1]).

%% Internal exports
-export([init/1,
         install/1]).

%% Debugging exports
-export([all/0,
         count_by_level/1,
         count/0]).

-export_type([rmodule/0,
              rmodule_map/0,
              object_id/0]).

-include("tables.hrl").

-record(introflection_modules, {object_id :: object_id() | '_',
                                name      :: name() | '_',
                                nesting   :: nesting(),
                                parent    :: object_id() | '_'}).

-type object_id() :: pos_integer().
-type name() :: string().
-type nesting() :: non_neg_integer().
-type rmodule() :: {object_id(), name(), nesting(), object_id()}.
-type rmodule_map() :: map().

%% ===================================================================
%% API functions
%% ===================================================================

-spec add(Module) -> ok when
      Module :: rmodule().

add({ObjectId, Name, Nesting, Parent}) ->
    F = fun() ->
        mnesia:write(#introflection_modules{object_id=ObjectId,
                                            name=Name,
                                            nesting=Nesting,
                                            parent=Parent})
    end,
    {atomic, ResultOfFun} = mnesia:transaction(F),
    ResultOfFun.

-spec find(ObjectId) -> any() when
      ObjectId :: object_id().

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

-spec annotate(Module) -> ModuleMap when
      Module :: rmodule(),
      ModuleMap :: rmodule_map().

annotate({ObjectId, Name, Nesting, Parent}) ->
    #{object_id => ObjectId,
      name => Name,
      nesting => Nesting,
      parent => Parent}.

-spec deannotate(ModuleMap) -> Module when
     ModuleMap :: rmodule_map(),
     Module :: rmodule().

deannotate(ModuleMap) ->
    #{object_id := O, name := N, nesting := G, parent := P} = ModuleMap,
    {O, N, G, P}.

-spec top_level() -> TopLevelModules when
      TopLevelModules :: [#introflection_modules{}].

top_level() ->
    F = fun() ->
        mnesia:match_object(#introflection_modules{nesting=0, _='_'})
    end,
    {atomic, ResultOfFun} = mnesia:transaction(F),
    ResultOfFun.

get_object_id(#introflection_modules{}=Record) ->
    Record#introflection_modules.object_id.

-spec is_annotated(Module) -> Boolean when
      Module :: rmodule() | rmodule_map(),
      Boolean :: boolean().

is_annotated({_, _, _, _}) ->
    false;
is_annotated(#{object_id := _, name := _, nesting := _, parent := _}) ->
    true.

%% ===================================================================
%% Internal functions
%% ===================================================================

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

%% ===================================================================
%% Debugging functions
%% ===================================================================

-spec all() -> Result when
      Result :: [rmodule()].

all() ->
    F = fun() ->
        mnesia:select(introflection_modules, [{'_', [], ['$_']}])
    end,
    {atomic, ResultOfFun} = mnesia:transaction(F),
    ResultOfFun.

-spec count_by_level(Level) -> Result when
      Level :: nesting(),
      Result :: non_neg_integer().

count_by_level(Level) ->
    F = fun() ->
        length(mnesia:match_object(#introflection_modules{nesting=Level, _='_'}))
    end,
    {atomic, ResultOfFun} = mnesia:transaction(F),
    ResultOfFun.

-spec count() -> Result when
      Result :: non_neg_integer().

count() ->
    length(all()).
