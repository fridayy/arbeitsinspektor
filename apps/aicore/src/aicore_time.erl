%%%=============================================================================
%%% @doc aicore_time
%%% @end
%%%=============================================================================
-module(aicore_time).

-export([now_utc/0, is_in_day_range/3, is_in_time_range/3]).

now_utc() -> calendar:universal_time().

is_in_day_range(Day, FromDay, ToDay) ->
  Day >= FromDay andalso Day =< ToDay.

is_in_time_range(Time, FromTime, ToTime) ->
  Time >= FromTime andalso Time =< ToTime.
