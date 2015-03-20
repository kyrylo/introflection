-module(introflection_websocket).

-behaviour(cowboy_websocket).

-include("introflection.hrl").

%% ===================================================================
%% cowboy_websocket callbacks
%% ===================================================================

-export([init/2, websocket_handle/3, websocket_info/3]).

-include("logger.hrl").

-define(SUBPROTO, "ifproto").
-define(SECWSP, "sec-websocket-protocol").

%% ===================================================================
%% cowboy_websocket callbacks
%% ===================================================================

init(Req, Opts) ->
    case cowboy_req:parse_header(<<?SECWSP>>, Req) of
        undefined ->
            {ok, Req, Opts};
        Subprotocols ->
            case lists:member(<<?SUBPROTO>>, Subprotocols) of
                true ->
                    ?INFO("Accepted a websocket connection"),
                    gproc:reg({p, l, {?MODULE, ?WSBCAST}}),
                    Req2 = cowboy_req:set_resp_header(<<?SECWSP>>, <<?SUBPROTO>>,
                                                      Req),
                    self() ! post_init,
                    {cowboy_websocket, Req2, Opts};
                false ->
                    {cowboy_websocket, Req, Opts}
            end
    end.

websocket_handle({text, Msg}, Req, State) ->
    {reply, {text, << "You said: ", Msg/binary >>}, Req, State};
websocket_handle(_Data, Req, State) ->
    {ok, Req, State}.

websocket_info(post_init, Req, State) ->
    MsgList = [{text, introflection_event:encode(Event) } ||
                  Event <- introflection_event:all_module_added()],
    {reply, MsgList, Req, State};
websocket_info({_Pid, {_Module, ?WSBCAST}, Msg}, Req, State) ->
    {reply, {text, Msg}, Req, State};
websocket_info(_Info, Req, State) ->
    {ok, Req, State}.
