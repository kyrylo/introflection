-module(introflection_websocket).

-behaviour(cowboy_websocket).

%% cowboy_websocket callbacks
-export([init/2,
         websocket_handle/3,
         websocket_info/3]).

-include("introflection.hrl").
-include("logger.hrl").

-define(SUBPROTO, "ifproto").
-define(SECWSP, "sec-websocket-protocol").

-type ws_init_ret() ::
        {ok, cowboy_req:req(), {}}
      | {cowboy_websocket, cowboy_req:req(), {}}.

-type ws_handle_ret() ::
        {ok, cowboy_req:req(), {}}
      | {reply, {text,<<_:64,_:_*8>>}, cowboy_req:req(), {}}.

%% ===================================================================
%% cowboy_websocket callbacks
%% ===================================================================

-spec init(cowboy_req:req(), term()) -> ws_init_ret().

init(Req, State) ->
    case cowboy_req:parse_header(<<?SECWSP>>, Req) of
        undefined ->
            {ok, Req, State};
        Subprotocols ->
            case lists:member(<<?SUBPROTO>>, Subprotocols) of
                true ->
                    ?INFO("Accepted a websocket connection"),
                    gproc:reg({p, l, {?MODULE, ?WSBCAST}}),
                    Req2 = cowboy_req:set_resp_header(<<?SECWSP>>, <<?SUBPROTO>>,
                                                      Req),
                    self() ! post_init,
                    {cowboy_websocket, Req2, State};
                false ->
                    {cowboy_websocket, Req, State}
            end
    end.

-spec websocket_handle(term(), term(), term()) -> ws_handle_ret().

websocket_handle({text, Msg}, Req, State) ->
    {reply, {text, << "You said: ", Msg/binary >>}, Req, State};
websocket_handle(_Data, Req, State) ->
    {ok, Req, State}.

-spec websocket_info(post_init, cowboy_req:req(), {}) ->
                        {reply, [{text, binary()}], cowboy_req:req(), term()};
                    ({pid(), {_,_}, binary()}, cowboy_req:req(), {}) ->
                        {reply, {text, binary()}, cowboy_req:req(), {}}.

websocket_info(post_init, Req, State) ->
    MsgList = [{text, encode(Event) } || Event <- introflection_event:modadds()],
    {reply, MsgList, Req, State};
websocket_info({_Pid, {_Module, ?WSBCAST}, Msg}, Req, State) ->
    {reply, {text, Msg}, Req, State};
websocket_info(_Info, Req, State) ->
    {ok, Req, State}.

%% ===================================================================
%% Internal functions
%% ===================================================================

-spec encode(term()) -> binary().

encode(Term) ->
    jiffy:encode(Term, [force_utf8]).
