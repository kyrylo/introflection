-module(introflection_event).

-include("introflection.hrl").
-include("events.hrl").
-include("logger.hrl").

%% API
-export([parse/1]).

%% ===================================================================
%% API functions
%% ===================================================================

parse(Bin) ->
    case Bin of
        <<?EVT_HEADER, Data/binary>> ->
            parse(parse_event(Data));
        Rest ->
            Rest
    end.

%% ===================================================================
%% Internal functions
%% ===================================================================

parse_event(Bin) ->
    case Bin of
        <<?MODULE_ADDED, Data/binary>> ->
            parse_event(parse_module_added(Data));
        Rest ->
            Rest
    end.

parse_module_added(Bin) ->
    case Bin of
        <<DataSize:8/integer, Data:DataSize/binary, ?EVT_DELIM, Rest/binary>> ->
            {ok, RTerm} = rmarshal:load(Data),
            broadcast(jiffy:encode(hd(RTerm), [force_utf8])),
            Rest;
        Rest ->
            Rest
    end.

broadcast(Data) ->
    gproc:send({p, l, {introflection_websocket, ?WSBCAST}},
               {self(), {introflection_websocket, ?WSBCAST}, Data}).
