-module(introflection_tcpserver).

-behaviour(gen_server).

-include("otp_types.hrl").
-include("introflection.hrl").
-include("logger.hrl").

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).
-define(TCP_PORT, 7331).
-define(TCP_OPTIONS, [binary, {packet, 0},
                      {reuseaddr, true},
                      {active, once}]).
-define(EVENT_MODULE_ADDED, 0).
-define(START_MESSAGE, "introflection").
-define(END_MESSAGE, "\r\n").


%% ===================================================================
%% API functions
%% ===================================================================

-spec(start_link() -> gs_init_reply()).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% ===================================================================
%% gen_server callbacks
%% ===================================================================

-spec(init(gs_args()) -> gs_init_reply()).

init([]) ->
    ?INFO("Spawning a TCP server"),
    spawn(fun() -> listen() end),
    {ok, {}}.

-spec(handle_call(gs_request(), gs_from(), gs_reply()) -> gs_call_reply()).

handle_call(_Request, _From, State) ->
    {noreply, State}.

-spec(handle_cast(gs_request(), gs_state()) -> gs_cast_reply()).

handle_cast(_Msg, State) ->
    {noreply, State}.

-spec(handle_info(gs_request(), gs_state()) -> gs_info_reply()).

handle_info(_Info, State) ->
    {noreply, State}.

-spec(terminate(terminate_reason(), gs_state()) -> ok).

terminate(_Reason, _State) ->
    ok.

-spec(code_change(term(), term(), term()) -> gs_code_change_reply()).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ===================================================================
%% Internal functions
%% ===================================================================

listen() ->
    case gen_tcp:listen(?TCP_PORT, ?TCP_OPTIONS) of
        {ok, ListenSock} ->
            ?INFO("Socket ~p has started listening on port ~p",
                  [ListenSock, ?TCP_PORT]),
            accept(ListenSock);
        {error, Reason} ->
            {error, Reason}
    end.

accept(ListenSock) ->
    case gen_tcp:accept(ListenSock) of
        {ok, Socket} ->
            ?INFO("Socket ~p accepted a new TCP connection", [Socket]),
            F = fun() -> loop(Socket) end,
            case gen_tcp:controlling_process(Socket, spawn(F)) of
                ok ->
                    accept(ListenSock);
                {error, Reason} ->
                    ?ERROR("Error: ~p", [Reason])
            end;
        Other ->
            ?ERROR("Accept returned ~w. Aborting!~n", [Other]),
            ok
    end.

loop(Socket) ->
    loop(Socket, <<>>).

loop(Socket, Rest) ->
    inet:setopts(Socket, [{active, once}]),
    receive
        {tcp, S, Data} ->
            loop(S, parse_message(<<Rest/binary, Data/binary>>));
        {tcp_closed, S} ->
            ?INFO("The TCP connection on socket ~p was closed", [S]),
            ok;
        {tcp_error, S, Reason} ->
            ?ERROR("Error on socket ~p reason: ~p~n", [S, Reason])
    end.

parse_message(Bin) ->
    case Bin of
        <<?START_MESSAGE, Data/binary>> ->
            case parse_event(Data) of
                {nomatch, _Rest} ->
                    Bin;
                {match, Rest} ->
                    parse_message(Rest)
            end;
        Rest ->
            Rest
    end.

parse_event(Bin) ->
    case Bin of
        <<?EVENT_MODULE_ADDED, Data/binary>> ->
            case parse_module_added(Data) of
                {nomatch, _Rest} ->
                    {nomatch, Bin};
                {match, Rest} ->
                    {match, Rest}
            end;
        Rest ->
            Rest
    end.

parse_module_added(Bin) ->
    case Bin of
        <<DataSize:8/integer, Data:DataSize/binary, ?END_MESSAGE, Rest/binary>> ->
            {ok, [Event]} = rmarshal:load(Data),
            {ok, Module} = maps:find(data, Event),
            ok = introflection_module:add(
                   maps:get(object_id, Module),
                   maps:get(name, Module),
                   maps:get(nesting, Module),
                   maps:get(parent, Module)
                  ),
            %%broadcast(jiffy:encode(Event, [force_utf8])),
            {match, Rest};
        Rest ->
            {nomatch, Rest}
    end.

broadcast(Data) ->
    gproc:send({p, l, {introflection_websocket, ?WSBCAST}},
               {self(), {introflection_websocket, ?WSBCAST}, Data}).
