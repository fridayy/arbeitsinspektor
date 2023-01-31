%%% % @noformat
%%% 
%%%=============================================================================
%%% @doc aicore_bhd
%%% This module has the responsilbity to check wether
%%% the given business hour definition is matched or not.
%%%
%%% For example:
%%% - given current date time: monday 10:00
%%%   mon-tue[08:00-17:00] -> true
%%%   mon-tue[11:00-17:00] -> false
%%%
%%% @end
%%%=============================================================================
-module(aicore_bhd).

-feature(maybe_expr, enable).

-export([match/1]).

-include_lib("kernel/include/logger.hrl").

%% @doc
%% Checks if the current UTC time is within the given business hour definiton.
%% @end
-spec match(BusinessHourDefinition) -> {ok, Matches} | {error, Reason} when
    BusinessHourDefinition :: binary() | string(),
    Reason :: any(),
    Matches :: allowed | not_allowed.
match(BusinessHourDefinition) when is_list(BusinessHourDefinition) -> 
   maybe
    {ok, Tokens, _} ?= aicore_bhd_lexer:string(BusinessHourDefinition),
    {ok, R} ?= aicore_bhd_parser:parse(Tokens),
    DateTimeNowUtc = aicore_time:now_utc(),
    ?LOG_DEBUG(#{event => check_allowance, current_datetime => DateTimeNowUtc, bhd => R}),
    is_allowed(DateTimeNowUtc, R)
   end;


match(BusinessHourDefinition) when is_binary(BusinessHourDefinition) ->
  match(binary_to_list(BusinessHourDefinition)).

is_allowed({DateNowUtc, TimeNowUtc}, #{day_range := {FromDay,ToDay}, from_time := FromTime, to_time := ToTime}) ->
  CurrentDay = calendar:day_of_the_week(DateNowUtc),
  maybe
    true ?= aicore_time:is_in_day_range(CurrentDay, FromDay, ToDay),
    true ?= aicore_time:is_in_time_range(TimeNowUtc, FromTime, ToTime),
    {ok, allowed}
  else
    _ -> {ok, not_allowed}
  end;

is_allowed(DateTimeNowUtc, #{day_single := Day, from_time := FromTime, to_time := ToTime}) ->
  is_allowed(DateTimeNowUtc, #{day_range => {Day,Day}, from_time => FromTime, to_time => ToTime}).

