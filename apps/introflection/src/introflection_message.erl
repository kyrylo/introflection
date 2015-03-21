-module(introflection_message).

-include("introflection.hrl").
-include("events.hrl").
-include("logger.hrl").

-export([parse/1]).

-define(START_MESSAGE, "introflection").
-define(END_MESSAGE, "\r\n").

parse(Bin) ->
    parse(Bin, []).

parse(Bin, Events) ->
    case Bin of
        <<?START_MESSAGE, Data/binary>> ->
            case parse_event(Data) of
                {nomatch, _Rest} ->
                    {ok, {Events, Bin}};
                {match, Event, Rest} ->
                    parse(Rest, [Event|Events])
            end;
        Rest ->
            {ok, {Events, Rest}}
    end.

parse_event(Bin) ->
    case Bin of
        <<?MODULE_ADDED, Data/binary>> ->
            case parse_module_added(Data) of
                {nomatch, _Rest} ->
                    {nomatch, Bin};
                {match, Event, Rest} ->
                    {match, Event, Rest}
            end;
        Rest ->
            Rest
    end.

parse_module_added(Bin) ->
    case Bin of
        <<DataSize:8/integer, Data:DataSize/binary, ?END_MESSAGE, Rest/binary>> ->
            {ok, [Event]} = rmarshal:load(Data),
            {match, Event, Rest};
        Rest ->
            {nomatch, Rest}
    end.
