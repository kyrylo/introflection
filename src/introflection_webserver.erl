-module(introflection_webserver).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-include("otp_types.hrl").

%% ===================================================================
%% API functions
%% ===================================================================

-spec start_link() -> Result when
      Result :: gs_start_link_reply().

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% ===================================================================
%% gen_server callbacks
%% ===================================================================

-spec init([]) -> {ok, {}}.

init([]) ->
    Host = '_',
    Routes = [
        {"/", cowboy_static, {priv_file, introflection, "/index.html"}},
        {"/ws", introflection_websocket, []},
        {"/assets/[...]", cowboy_static, {priv_dir, introflection, "dist/"}},
        {"/deps/[...]", cowboy_static, {priv_dir, introflection, "bower_components/"}}
    ],
    Dispatch = cowboy_router:compile([{Host, Routes}]),
    cowboy:start_http(introflection, 100,
                      [{port, 8080}],
                      [{env, [{dispatch, Dispatch}]}]),

    {ok, {}}.

-spec handle_call(gs_request(), gs_from(), gs_reply()) -> {noreply, {}}.

handle_call(_Request, _From, State) ->
    {noreply, State}.

-spec handle_cast(gs_request(), gs_state()) -> {noreply, {}}.

handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(gs_request(), gs_state()) -> {noreply, {}}.

handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(gs_terminate_reason(), gs_state()) -> ok.

terminate(_Reason, _State) ->
    cowboy:stop_listener(introflection).

-spec code_change(term(), term(), term()) -> {ok, {}}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
