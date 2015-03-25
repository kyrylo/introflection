-module(introflection_module_space).

-behaviour(gen_event).

%% API

%% gen_server callbacks
-export([init/1,
         handle_event/2,
         handle_call/2,
         handle_info/2,
         code_change/3,
         terminate/2]).

-include("introflection.hrl").
-include("logger.hrl").

-record(state, {digraph,
                orphans=[]}).

%% ===================================================================
%% API functions
%% ===================================================================

%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init([]) ->
    Digraph = digraph:new([acyclic]),
    {ok, #state{digraph=Digraph}}.

handle_event({add_module, Module}, State) ->
    {ok, Orphans} = draw_edges(Module, State),
    {ok, State#state{orphans=Orphans}};
%% gproc:send({p, l, {introflection_websocket, ?WSBCAST}},
%%            {self(), {introflection_websocket, ?WSBCAST},
%%             jiffy:encode(Module, [force_utf8])}),
handle_event(_From, State) ->
    {ok, State}.

handle_call(_Msg, State) ->
    {ok, ok, State}.

handle_info(_Info, State) ->
    {ok, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

draw_edges(Module, State) ->
    Digraph = State#state.digraph,
    Orphans = State#state.orphans,
    draw_edges(Digraph, Orphans, Module).

draw_edges(Digraph, Orphans, {ObjectId, Name, _G, _P}=Module) ->
    digraph:add_vertex(Digraph, ObjectId, Name),
    {ok, POrphans} = link_parent(Digraph, Orphans, Module),
    link_children(Digraph, POrphans, Module).

link_parent(Digraph, Orphans, {Child, _N, _G, Parent}=Module) ->
    case digraph:vertex(Digraph, Parent) of
        {_Vertex, _Label} ->
            if
                Child =/= Parent ->
                    case digraph:get_path(Digraph, Child, Parent) of
                        false ->
                            digraph:add_edge(Digraph, Child, Parent),
                            ParentMod = introflection_module:find(Parent),
                            link_parent(Digraph, Orphans, ParentMod);
                        _ ->
                          {ok, Orphans}
                   end;
                true ->
                    {ok, Orphans}
            end;
        false ->
            case lists:member(Module, Orphans) of
                true -> {ok, Orphans};
                false -> {ok, [Module|Orphans]}
            end
    end.

link_children(Digraph, Orphans, {Parent, _N, _G, _Grandparent}) ->
    Children = [ChildMod || {_,_,_,ParId}=ChildMod <- Orphans, Parent =:= ParId],
    NewOrphans = Orphans -- Children,
    lists:foreach(fun({Child, _, _, _P}=ChildMod) ->
                      digraph:add_edge(Digraph, Child, Parent),
                      {ok, NewOrphans} = link_children(Digraph, NewOrphans, ChildMod),
                      NewOrphans
                  end, Children),
    {ok, NewOrphans}.
