-module(introflection_message).

%% API
-export([parse/1]).

-include("introflection.hrl").
-include("events.hrl").

-define(START_MESSAGE, "introflection").
-define(END_MESSAGE, "\r\n").


%% ===================================================================
%% API functions
%% ===================================================================

-spec parse(Data) -> {ok, {EventList, Leftover}} when
      Data :: binary(),
      EventList :: [introflection_event:event()],
      Leftover :: binary().

parse(Bin) ->
    parse(Bin, []).

%% ===================================================================
%% Internal functions
%% ===================================================================

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

-spec parse_event(Data) -> {nomatch, Data} | {match, Event, Leftover} when
      Data :: binary(),
      Event :: introflection_event:event(),
      Leftover :: binary().

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
            {nomatch, Rest}
    end.

-spec parse_module_added(Data) -> {nomatch, Data} | {match, Event, Leftover} when
      Data :: binary(),
      Event :: introflection_event:event(),
      Leftover :: binary().

parse_module_added(Bin) ->
    case Bin of
        <<DataSize:8/integer, Data:DataSize/binary, ?END_MESSAGE, Rest/binary>> ->
            {ok, [Event]} = rmarshal:load(Data),
            {match, Event, Rest};
        Rest ->
            {nomatch, Rest}
    end.
