%%%=============================================================================
%%% @doc aicore
%%% @end
%%%=============================================================================
-module(aicore).

-export([run_actions/0]).

-type recommendation() ::
    {ok, no_action_required} | {ok, {scale, 0}} | {ok, scale, 1} | {error, {unknown, any()}}.

-record(recommendation, {
    name :: binary(),
    namespace :: binary(),
    recommendation :: recommendation(),
    business_hours :: binary()
}).

run_actions() ->
    BusinessHourDefinitions = aicore_kubernetes:business_hours_definitions(),
    warn_if_no_definitions(BusinessHourDefinitions),
    Recommendations = lists:flatmap(
        fun(#{business_hours := BusinessHours, managed_resources := ManagedResources}) ->
            lists:map(
                fun(ManagedResource) ->
                    Recommendation = recommend_action(BusinessHours, ManagedResource),
                    #recommendation{
                        name = aicore_kubernetes:name(ManagedResource),
                        namespace = aicore_kubernetes:namespace(ManagedResource),
                        recommendation = Recommendation,
                        business_hours = BusinessHours
                    }
                end,
                ManagedResources
            )
        end,
        BusinessHourDefinitions
    ),
    lists:foreach(fun follow_recommendation/1, Recommendations),
    c:q().

warn_if_no_definitions([]) -> logger:warning("No business hour definitions found - nothing to do.");
warn_if_no_definitions(_) -> ok.

%% internal functions
recommend_action(BusinessHours, #{current_replicas := CurrentReplicas}) ->
    case aicore_bhd:match(BusinessHours) of
        {ok, not_allowed} when CurrentReplicas =:= 0 ->
            {ok, already_scaled_down};
        {ok, not_allowed} when CurrentReplicas > 0 ->
            {ok, {scale, 0}};
        {ok, allowed} when CurrentReplicas =:= 0 ->
            {ok, {scale, 1}};
        {ok, allowed} when CurrentReplicas > 0 ->
            {ok, already_running};
        Else ->
            {error, {unknown, Else}}
    end.

follow_recommendation(#recommendation{
    name = Name,
    namespace = Namespace,
    recommendation = {ok, {scale, ScaleTo}},
    business_hours = BusinessHours
}) ->
    NowStr = aicore_time:human_readable_now(),
    logger:info(
        "Following recommendation to scale '~s' to ~B replicas [business_hours = '~s' | now = '~s']",
        [Name, ScaleTo, BusinessHours, NowStr]
    ),
    case is_dry_run() of
        true ->
            logger:warning("No action taken [dry_run=true]"),
            ok;
        false ->
            aicore_kubernetes:scale(Name, Namespace, ScaleTo)
    end;
follow_recommendation(#recommendation{
    name = Name, business_hours = BusinessHours, recommendation = {error, Reason}
}) ->
    NowStr = aicore_time:human_readable_now(),
    logger:error(
        "Could not process '~s' due to an error: ~p [business_hours = '~s' | now = '~s']", [
            Name, Reason, BusinessHours, NowStr
        ]
    ),
    ok;
follow_recommendation(#recommendation{
    name = Name, business_hours = BusinessHours, recommendation = {ok, already_running}
}) ->
    NowStr = aicore_time:human_readable_now(),
    logger:info(
        "No action required for '~s' as it runs at desired replica count > 0 [business_hours = '~s' | now = '~s']",
        [Name, BusinessHours, NowStr]
    ),
    ok;
follow_recommendation(#recommendation{
    name = Name, business_hours = BusinessHours, recommendation = {ok, already_scaled_down}
}) ->
    NowStr = aicore_time:human_readable_now(),
    logger:info(
        "No action required for '~s' as it is not running [business_hours = '~s' | now = '~s']", [
            Name, BusinessHours, NowStr
        ]
    ),
    ok.

is_dry_run() ->
    X = application:get_env(aicore, dry_run),
    case X of
        undefined -> false;
        {ok, "true"} -> true;
        {ok, "false"} -> false;
        {ok, _} -> false
    end.
