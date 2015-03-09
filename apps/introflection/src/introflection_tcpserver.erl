-module(introflection_tcpserver).

-behaviour(gen_server).

-include("otp_types.hrl").
-include("logger.hrl").

-import(introflection_event, [parse/1]).

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
    {ok, ListenPort} = gen_tcp:listen(?TCP_PORT, ?TCP_OPTIONS),
    ?INFO("Started listening on port ~p", [?TCP_PORT]),
    accept(ListenPort).

accept(ListenPort) ->
    {ok, Socket} = gen_tcp:accept(ListenPort),
    ?INFO("Accepted a new TCP connection"),
    gen_tcp:controlling_process(Socket, spawn(fun() -> loop(Socket) end)),
    accept(ListenPort).

loop(Socket) ->
    loop(Socket, <<>>).

loop(Socket, Rest) ->
    inet:setopts(Socket, [{active, once}]),
    receive
        {tcp, Socket, Data} ->
            loop(Socket, parse(<<Data/binary, Rest/binary>>));
        {tcp_closed, Socket} ->
            ?INFO("The TCP connection was closed"),
            ok;
        {tcp_error, Socket, Reason} ->
            ?ERROR("Error on socket ~p reason: ~p~n", [Socket, Reason])
    end.
