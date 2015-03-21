-module(introflection_tcpserver).

-behaviour(gen_server).

-include("otp_types.hrl").
-include("logger.hrl").

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {socket,
                leftover=nil}).

%% ===================================================================
%% API functions
%% ===================================================================

-spec(start_link(socket()) -> gs_init_reply()).

start_link(Socket) ->
    gen_server:start_link(?MODULE, Socket, []).

%% ===================================================================
%% gen_server callbacks
%% ===================================================================

-spec(init(gs_args()) -> gs_init_reply()).

init(Socket) ->
    ?INFO("Spawning a TCP server"),
    gen_server:cast(self(), accept),
    {ok, #state{socket=Socket}}.

-spec(handle_call(gs_request(), gs_from(), gs_reply()) -> gs_call_reply()).

handle_call(_Request, _From, State) ->
    {noreply, State}.

-spec(handle_cast(gs_request(), gs_state()) -> gs_cast_reply()).

handle_cast(accept, State = #state{socket=ListenSocket}) ->
    case gen_tcp:accept(ListenSocket) of
        {ok, AcceptSocket} ->
            ?INFO("Socket ~p accepted a new TCP connection", [AcceptSocket]),
            introflection_tcpserver_sup:start_socket(),
            {noreply, State#state{socket=AcceptSocket}};
        Other ->
            ?ERROR("Accept returned ~w. Aborting!~n", [Other]),
            ok
    end;
handle_cast({handle_events, Events}, State) ->
    ok = introflection_event:bulk_store(Events),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec(handle_info(gs_request(), gs_state()) -> gs_info_reply()).

handle_info({tcp, Socket, Data}, State = #state{leftover=Rest}) when Rest /= nil ->
    {ok, NewState} = handle_streaming_data(<<Rest/binary, Data/binary>>, State),
    inet:setopts(Socket, [{active, once}]),
    {noreply, NewState};
handle_info({tcp, Socket, Data}, State) ->
    {ok, NewState} = handle_streaming_data(Data, State),
    inet:setopts(Socket, [{active, once}]),
    {noreply, NewState};
handle_info({tcp_closed, Socket}, State) ->
    ?INFO("The TCP connection on socket ~p was closed", [Socket]),
    {stop, normal, State};
handle_info({tcp_error, Socket, Reason}, State) ->
    ?ERROR("Error on socket ~p reason: ~p~n", [Socket, Reason]),
    {stop, normal, State};
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

handle_streaming_data(Data, State) ->
    {ok, {Events, Leftover}} = introflection_message:parse(Data),
    gen_server:cast(self(), {handle_events, Events}),
    {ok, State#state{leftover=Leftover}}.
