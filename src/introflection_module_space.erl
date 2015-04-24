-module(introflection_module_space).

-behaviour(gen_event).

%% gen_server callbacks
-export([init/1,
         handle_event/2,
         handle_call/2,
         handle_info/2,
         code_change/3,
         terminate/2]).

-include("otp_types.hrl").
-include("logger.hrl").
-include("events.hrl").

-record(state, {digraph,
                orphans=[]}).

%% ===================================================================
%% gen_server callbacks
%% ===================================================================

-spec init([]) -> {ok, #state{digraph :: digraph:graph(),
                              orphans :: []}}.

init([]) ->
    Digraph = digraph:new([acyclic]),
    {ok, #state{digraph=Digraph}}.

-spec handle_event(ge_event(), ge_state()) -> {ok, #state{}}.

handle_event({add_module, {ObjectId,_,_,_}=Module}, State) ->
    timer:sleep(10), %% Dirty hack. Without this not all modules will be sent.
    {ok, Orphans} = draw_edges(Module, State),

    case introflection_module:top_level() of
        [] ->
            {ok, State#state{orphans=[Module|Orphans]}};
        [RootModule|_TopLevelModules] ->
            RootId = introflection_module:get_object_id(RootModule),
            Digraph = State#state.digraph,
            NewOrphans = emit_orphans(Digraph, State#state.orphans, RootId),
            OrphansList = NewOrphans ++ Orphans,
            case emit_chain(Digraph, ObjectId, RootId) of
                ok ->
                    {ok, State#state{orphans=lists:usort(OrphansList)}};
                not_ok ->
                    {ok, State#state{orphans=lists:usort([Module|OrphansList])}}
            end
    end;
handle_event(_From, State) ->
    {ok, State}.

-spec handle_call(ge_request(), ge_state()) -> {ok, ok, #state{}}.

handle_call(_Msg, State) ->
    {ok, ok, State}.

-spec handle_info(ge_request(), ge_state()) -> {ok, #state{}}.

handle_info(_Info, State) ->
    {ok, State}.

-spec code_change(term(), term(), term()) -> ge_code_change_reply().

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

-spec terminate(ge_terminate_reason(), ge_state()) -> ok.

terminate(_Reason, _State) ->
    ok.

%% ===================================================================
%% Private functions
%% ===================================================================

-spec draw_edges(Module, State) -> {ok, Orphans} when
      Module :: introflection_module:rmodule(),
      State :: ge_state(),
      Orphans :: [Module].

draw_edges(M, State) ->
    Digraph = State#state.digraph,
    Orphans = State#state.orphans,
    draw_edges(Digraph, Orphans, M).

-spec draw_edges(Digraph, Orphans, Module) -> {ok, NewOrphans} when
      Digraph :: digraph:graph(),
      Orphans :: [introflection_module:rmodule()],
      Module :: introflection_module:rmodule(),
      NewOrphans :: [introflection_module:rmodule()].

draw_edges(D, O, {I, N,_,_}=M) ->
    digraph:add_vertex(D, I, N),
    {ok, PO} = link_parent(D, O, M),
    link_children(D, PO, M).

-spec link_parent(Digraph, Orphans, Module) -> {ok, NewOrphans} when
      Digraph :: digraph:graph(),
      Orphans :: [introflection_module:rmodule()],
      Module :: introflection_module:rmodule(),
      NewOrphans :: [Module].

link_parent(D, O, {I,_,_,P}=M) ->
    case digraph:vertex(D, P) of
        {_V, _L} ->
            if
                I =/= P ->
                    case digraph:get_path(D, I, P) of
                        false ->
                            digraph:add_edge(D, I, P),
                            PM = introflection_module:find(P),
                            link_parent(D, O, PM);
                        _ ->
                            {ok, O}
                    end;
                true ->
                    {ok, O}
            end;
        false ->
            case lists:member(M, O) of
                true -> {ok, O};
                false -> {ok, [M|O]}
            end
    end.

-spec link_children(Digraph, Orphans, Module) -> {ok, NewOrphans} when
      Digraph :: digraph:graph(),
      Orphans :: [introflection_module:rmodule()],
      Module :: introflection_module:rmodule(),
      NewOrphans :: [Module].

link_children(D, O, {I,_,_,_}) ->
    Children = [CM || {_,_,_,GI}=CM <- O, I =:= GI],
    NewO = link_grandchildren(D, O -- Children, Children, I),
    {ok, NewO}.

link_grandchildren(D, O, [{I,_,_,G}=M|Children], P) ->
    digraph:add_edge(D, I, P),
    {ok, NewO} = link_children(D, O, M),
    link_grandchildren(D, NewO, Children, G);
link_grandchildren(_Digraph, Orphans, [], _ParentId) ->
    Orphans.

-spec emit_chain(Digraph, ChildId, RootId) -> not_ok | ok when
      Digraph :: digraph:graph(),
      ChildId :: introflection_module:object_id(),
      RootId :: introflection_module:object_id().

emit_chain(D, C, R) ->
    case digraph:get_path(D, C, R) of
        false ->
            not_ok;
        IdChain ->
            Chain = [introflection_module:find(I) || I <- IdChain],
            introflection_event:emit(?MODULE_CHAIN_ADDED, Chain),
            ok
    end.

-spec emit_orphans(Digraph, Orphans, RootId) -> Carry when
      Digraph :: digraph:graph(),
      Orphans :: [introflection_module:rmodule()],
      RootId :: introflection_module:object_id(),
      Carry :: [introflection_module:rmodule()].

emit_orphans(D, O, R) ->
    emit_orphans(D, O, R, []).

emit_orphans(D, [{I,_,_,_}=Orphan|O], R, Carry) ->
    case emit_chain(D, I, R) of
        ok ->
            emit_orphans(D, O, R, Carry);
        not_ok ->
            emit_orphans(D, O, R, [Orphan|Carry])
    end;
emit_orphans(_Digraph, [], _RootId, Carry) ->
    Carry.
