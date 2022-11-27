%%%=============================================================================
%%% @doc aicore_time
%%% @end
%%%=============================================================================
-module(aicore_time).

-export([now_utc/0, is_in_day_range/3, is_in_time_range/3, human_readable_now/0]).

now_utc() -> calendar:universal_time().

is_in_day_range(Day, FromDay, ToDay) ->
    Day >= FromDay andalso Day =< ToDay.

is_in_time_range(Time, FromTime, ToTime) ->
    Time >= FromTime andalso Time =< ToTime.

human_readable_now() ->
    {{Y, M, D}, {H, MM, S}} = calendar:universal_time(),
    Day = human_readable_day_of_week(Y, M, D),
    io_lib:format("~B-~B-~B ~B:~B:~B [~s]", [Y, M, D, H, MM, S, Day]).

human_readable_day_of_week(Y, M, D) ->
    Day = calendar:day_of_the_week(Y, M, D),
    to_day_str(Day).

to_day_str(1) -> "monday";
to_day_str(2) -> "tuesday";
to_day_str(3) -> "wednesday";
to_day_str(4) -> "thursday";
to_day_str(5) -> "friday";
to_day_str(6) -> "saturday";
to_day_str(7) -> "sunday".
