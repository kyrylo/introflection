-module(introflection_webserver).
-behaviour(gen_server).

-export([start_link/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

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

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    cowboy:stop_listener(introflection).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
