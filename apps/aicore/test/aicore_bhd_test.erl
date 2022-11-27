-module(aicore_bhd_test).

-include_lib("eunit/include/eunit.hrl").

match_test_() ->
    {foreach, fun setup/0, fun teardown/1, [
        fun lower_boundary_not_allowed/1,
        fun lower_boundary_allowed/1,
        fun day_does_not_match/1,
        fun single_day_allowed/1,
        fun single_day_not_allowed/1
    ]}.

setup() ->
    meck:new(aicore_time, [passthrough]),
    ok.

teardown(_) ->
    meck:unload(aicore_time).

lower_boundary_not_allowed(_) ->
    meck:expect(aicore_time, now_utc, fun() -> {{2022, 11, 21}, {7, 59, 59}} end),
    R = aicore_bhd:match("mon-fri[08:00-09:00]"),
    ?_assertEqual(R, {ok, not_allowed}).

lower_boundary_allowed(_) ->
    meck:expect(aicore_time, now_utc, fun() -> {{2022, 11, 21}, {8, 0, 0}} end),
    R = aicore_bhd:match("mon-fri[08:00-09:00]"),
    ?_assertEqual(R, {ok, allowed}).

day_does_not_match(_) ->
    meck:expect(aicore_time, now_utc, fun() -> {{2022, 11, 20}, {8, 30, 0}} end),
    R = aicore_bhd:match("mon-tue[08:00-09:00]"),
    ?_assertEqual(R, {ok, not_allowed}).

single_day_allowed(_) ->
    meck:expect(aicore_time, now_utc, fun() -> {{2022, 11, 21}, {8, 0, 0}} end),
    R = aicore_bhd:match("mon[08:00-09:00]"),
    ?_assertEqual(R, {ok, allowed}).

single_day_not_allowed(_) ->
    meck:expect(aicore_time, now_utc, fun() -> {{2022, 11, 20}, {8, 1, 0}} end),
    R = aicore_bhd:match("mon[08:00-09:00]"),
    ?_assertEqual(R, {ok, not_allowed}).
