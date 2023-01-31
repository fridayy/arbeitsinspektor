%%%=============================================================================
%%% @doc aicore
%%% @end
%%%=============================================================================
-module(aicore).

-include_lib("kernel/include/logger.hrl").
-include("aicore.hrl").

-ifdef(TEST).
-compile([export_all, nowarn_export_all]).
-endif.

-export([
    deployment/4,
    stateful_set/4,
    perform_inspection/0,
    bhd/2,
    apply_recommendations/1,
    name/1,
    namespace/1,
    kind/1,
    current_replicas/1,
    desired_replicas/1,
    perform_inspection_and_apply/0
]).

-type bhd() :: #business_hour_definition{}.
-type scale_target() :: #scale_target{}.
-type scale_targets() :: #scale_targets{}.

-export_type([bhd/0, scale_target/0, scale_targets/0]).

%% api

%% @doc
%% Performs an inspection and applies the resulting scaling recommendations.
%% @end
-spec perform_inspection_and_apply() -> ok.
perform_inspection_and_apply() ->
    Recommendations = perform_inspection(),
    apply_recommendations(Recommendations),
    ok.

%% @doc
%% Performs an inspection and returns a recommendation which workloads should be scaled
%% in order to meet the specified business hour definitions.
%% @end
-spec perform_inspection() -> list(scale_targets()).
perform_inspection() ->
    BusinessHourDefintions = aicore_kubernetes:business_hours_definitions(),
    ScaleTargets = get_scale_targets(BusinessHourDefintions),
    ?LOG_DEBUG(#{event => got_get_scale_targets, scale_targets => ScaleTargets}),
    recommend_actions(ScaleTargets).

%% @doc
%% Applies the given recommendations aka. a list of target workloads that should be scaled.
%% @end
-spec apply_recommendations(Recommendations) -> ok when
    Recommendations :: scale_targets().
apply_recommendations(Recommendations) ->
    case aicore_config:get_env_ensure(dry_run, true, boolean) of
        true ->
            ?LOG_INFO(#{
                event => dry_run_active,
                message => "'dry_run' is enabled - will not perform any actual scaling"
            }),
            lists:foreach(
                fun(ScaleTarget) ->
                    ?LOG_INFO(#{
                        event => scale,
                        message => io_lib:format(
                            "Will scale [~s] '~s' in namespace '~s' from ~p -> ~p",
                            [
                                ScaleTarget#scale_target.kind,
                                ScaleTarget#scale_target.name,
                                ScaleTarget#scale_target.namespace,
                                ScaleTarget#scale_target.current_replicas,
                                ScaleTarget#scale_target.desired_replicas
                            ]
                        )
                    })
                end,
                Recommendations
            ),
            ok;
        false ->
            ?LOG_INFO(#{
                event => performing_actions,
                message => "will proceed to perform necessary scale actions"
            }),
            aicore_kubernetes:scale(Recommendations),
            ?LOG_DEBUG(#{event => performed_actions}),
            ok
    end.

%% domain
-spec bhd(string(), map()) -> #business_hour_definition{}.
bhd(Spec, Labels) when is_list(Spec) andalso is_map(Labels) ->
    #business_hour_definition{spec = Spec, labels = Labels}.

-spec stateful_set(Name, Namespace, CurrentReplicas, DesiredReplicas) -> St when
    Name :: string(),
    Namespace :: string(),
    CurrentReplicas :: integer(),
    DesiredReplicas :: integer(),
    St :: scale_target().
stateful_set(Name, Namespace, CurrentReplicas, DesiredReplicas) ->
    #scale_target{
        name = Name,
        namespace = Namespace,
        kind = statefulset,
        current_replicas = CurrentReplicas,
        desired_replicas = DesiredReplicas
    }.

-spec deployment(Name, Namespace, CurrentReplicas, DesiredReplicas) -> St when
    Name :: string(),
    Namespace :: string(),
    CurrentReplicas :: integer(),
    DesiredReplicas :: integer(),
    St :: scale_target().
deployment(Name, Namespace, CurrentReplicas, DesiredReplicas) ->
    #scale_target{
        name = Name,
        namespace = Namespace,
        kind = deployment,
        current_replicas = CurrentReplicas,
        desired_replicas = DesiredReplicas
    }.

-spec name(ScaleTarget) -> string() when ScaleTarget :: aicore:scale_target().
name(#scale_target{name = Name}) -> Name.
namespace(#scale_target{namespace = Namespace}) -> Namespace.
current_replicas(#scale_target{current_replicas = CurrentReplicas}) -> CurrentReplicas.
desired_replicas(#scale_target{desired_replicas = DesiredReplicas}) -> DesiredReplicas.
kind(#scale_target{kind = Kind}) -> atom_to_list(Kind).

%% internal

-spec get_scale_targets(BusinessHourDefintions) -> Result when
    BusinessHourDefintions :: list(bhd()),
    Result :: list(#scale_targets{}).
get_scale_targets(BusinessHourDefintions) ->
    lists:map(
        fun(#business_hour_definition{spec = _Spec, labels = Labels} = Defintion) ->
            Targets = aicore_kubernetes:get_scale_targets(Labels),
            #scale_targets{
                targets = Targets,
                business_hour_definition = Defintion
            }
        end,
        BusinessHourDefintions
    ).

-spec recommend_actions(ScaleTargets) -> list() when
    ScaleTargets :: list(scale_targets()).
recommend_actions(ScaleTargets) ->
    RelevantTargets = lists:flatmap(
        fun(
            #scale_targets{
                targets = Targets,
                business_hour_definition = #business_hour_definition{spec = Definition}
            }
        ) ->
            {ok, Allowance} = aicore_bhd:match(Definition),
            lists:filtermap(fun(Target) -> map_target(Allowance, Target) end, Targets)
        end,
        ScaleTargets
    ),
    RelevantTargets.

map_target(not_allowed, #scale_target{current_replicas = R} = Y) when R > 0 ->
    {true, Y#scale_target{desired_replicas = 0}};
map_target(not_allowed, #scale_target{current_replicas = 0}) ->
    false;
map_target(allowed, #scale_target{current_replicas = R}) when R > 0 -> false;
map_target(allowed, #scale_target{current_replicas = 0, desired_replicas = undefined} = Y) ->
    {true, Y#scale_target{desired_replicas = 1}};
map_target(allowed, #scale_target{current_replicas = 0, desired_replicas = Desired} = Y) ->
    {true, Y#scale_target{desired_replicas = Desired}}.
