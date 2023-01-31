-module(aicore_core_test).

-author("bnjm").

-include_lib("eunit/include/eunit.hrl").
-include("aicore.hrl").

all_test_() ->
    {foreach, fun setup/0, fun teardown/1, [
        fun scale_down_all/1,
        fun scale_up_all/1,
        fun respect_already_set_desired_replicas_for_scaleup/1
    ]}.

setup() ->
    meck:new(aicore_time, [passthrough]),
    ok.

teardown(_) ->
    meck:unload(aicore_time).

scale_down_all(_) ->
    meck:expect(aicore_time, now_utc, fun() -> {{2022, 11, 21}, {7, 59, 59}} end),
    Defintion = aicore:bhd("mon-tue[09:00-18:00]", #{<<"leftshift.one/environment">> => <<"development">>}),
    Targets = [
                #scale_targets{
                                     targets = [#scale_target{
                                                  name = "target-0",
                                                  namespace = "test",
                                                  current_replicas = 1,
                                                  desired_replicas = 1
                                                 },
                                                #scale_target{
                                                   name = "target-1",
                                                   namespace = "test",
                                                   current_replicas = 1,
                                                   desired_replicas = undefined
                                                  }
                                               ],
                                     business_hour_definition = Defintion
                                    }
              ],
    R = aicore:recommend_actions(Targets),
    ?_assertMatch([#scale_target{current_replicas = 1, desired_replicas = 0}, 
                      #scale_target{current_replicas = 1, desired_replicas = 0}], R).

scale_up_all(_) ->
    meck:expect(aicore_time, now_utc, fun() -> {{2022, 11, 21}, {7, 59, 59}} end),
    Defintion = aicore:bhd("mon-tue[07:00-18:00]", #{<<"leftshift.one/environment">> => <<"development">>}),
    Targets = [
                #scale_targets{
                                     targets = [#scale_target{
                                                  name = "target-0",
                                                  namespace = "test",
                                                  current_replicas = 0,
                                                  desired_replicas = undefined
                                                 },
                                                #scale_target{
                                                   name = "target-1",
                                                   namespace = "test",
                                                   current_replicas = 0,
                                                   desired_replicas = undefined
                                                  }
                                               ],
                                     business_hour_definition = Defintion
                                    }
              ],
    R = aicore:recommend_actions(Targets),
    ?_assertMatch([#scale_target{current_replicas = 0, desired_replicas = 1}, 
                      #scale_target{current_replicas = 0, desired_replicas = 1}], R).

respect_already_set_desired_replicas_for_scaleup(_) ->
    meck:expect(aicore_time, now_utc, fun() -> {{2022, 11, 21}, {7, 59, 59}} end),
    Defintion = #business_hour_definition{
                     spec = "mon-tue[07:00-18:00]",
                     labels = #{<<"leftshift.one/environment">> => <<"development">>}
                    },
    Targets = [
                #scale_targets{
                                     targets = [#scale_target{
                                                  name = "target-0",
                                                  namespace = "test",
                                                  current_replicas = 0,
                                                  desired_replicas = 3
                                                 },
                                                #scale_target{
                                                   name = "target-1",
                                                   namespace = "test",
                                                   current_replicas = 0,
                                                   desired_replicas = undefined
                                                  }
                                               ],
                                     business_hour_definition = Defintion
                                    }
              ],
    R = aicore:recommend_actions(Targets),
    ?_assertMatch([#scale_target{current_replicas = 0, desired_replicas = 3}, 
                      #scale_target{current_replicas = 0, desired_replicas = 1}], R).
