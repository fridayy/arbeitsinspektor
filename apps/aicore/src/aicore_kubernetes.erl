%%%=============================================================================
%%% @doc aicore_kubernetes
%%%
%%% Encapsulates all kubernetes related functions. Uses kuberlnetes to
%%% perform the actual api server calls and caches the server configuration
%%% using an ets table bound to the process using this module (see: aicore_runner.erl)
%%%
%%% @end
%%%=============================================================================
-module(aicore_kubernetes).

-export([business_hours_definitions/0, get_scale_targets/1, scale/1]).

-include_lib("kernel/include/logger.hrl").
-define(TABLE, aicore_kubenertes_server_config).
-define(LAST_REPLICAS_ANNOTATION_KEY, <<"arbeitsinspektor.ghcr.io/last-replicas">>).

%% @doc
%% Returns a list of all managed resources - meaning resources that contain
%% arbeitsinspektor related annotations
%% @end
-spec business_hours_definitions() -> list(aicore:bhd()).
business_hours_definitions() ->
    BusinessHourDefinitions = kuberlnetes:get(
        "/apis/arbeitsinspektor.ghcr.io/v1/businesshours?limit=1", #{server => load_server_config()}
    ),
    %% there should only be only bhd object (limited by resource quotas)
    case items(BusinessHourDefinitions) of
        [] ->
            ?LOG_ERROR(#{
                error => no_business_hour_definition,
                message => "There is no business hour definiton custom resource in this cluster"
            }),
            error(no_business_hour_definition);
        [#{<<"spec">> := #{<<"definitions">> := Definitions}} | _] ->
            lists:map(
                fun(#{<<"definition">> := D, <<"label">> := L}) ->
                    aicore:bhd(erlang:binary_to_list(D), L)
                end,
                Definitions
            )
    end.

-spec get_scale_targets(Labels) -> Result when
    Labels :: map(),
    Result :: list(aicore:scale_target()).
get_scale_targets(Labels) ->
    ?LOG_DEBUG(#{event => get_scale_targets, labels => Labels}),
    Deployments = list_deployments(Labels),
    ?LOG_INFO(#{event => got_scale_targets, type => deployment, count => length(Deployments)}),
    lists:map(
        fun(Deployment) -> app_v1_resource_to_scale_target(Deployment, deployment) end, Deployments
    ).

-spec scale(list(aicore:scale_target())) -> ok.
scale(ScaleTargets) ->
    lists:map(
        fun(ScaleTarget) ->
            scale_resource(
                aicore:kind(ScaleTarget),
                aicore:name(ScaleTarget),
                aicore:namespace(ScaleTarget),
                aicore:desired_replicas(ScaleTarget)
            )
        end,
        ScaleTargets
    ),
    ok.

%% internal
load_server_config() ->
    case ets:info(?TABLE) of
        undefined ->
            ets:new(?TABLE, [set, private, named_table]),
            % ServerConfig = kuberlnetes:in_cluster(),
            ServerConfig = kuberlnetes:from_config(#{context => "bnjm-test"}),
            ets:insert(?TABLE, {ServerConfig}),
            ServerConfig;
        _ ->
            ets:first(?TABLE)
    end.

scale_resource(Resource, Name, Namespace, ScaleTo) when ScaleTo >= 0 ->
    Path = io_lib:format("/apis/apps/v1/namespaces/~s/~ss/~s/scale", [Namespace, Resource, Name]),
    ?LOG_DEBUG(#{method => "patch", path => Path, scale_to => ScaleTo}),
    kuberlnetes:patch(
        #{
            path => Path,
            body => #{
                <<"spec">> => #{<<"replicas">> => ScaleTo}
            }
        },
        #{server => load_server_config()}
    ).

list_deployments(Labels) ->
    list_app_v1_by_label("deployments", Labels).

list_stateful_sets(Labels) ->
    list_app_v1_by_label("statefulsets", Labels).

list_app_v1_by_label(Resource, Labels) when
    is_list(Resource) andalso is_map(Labels) andalso map_size(Labels) =:= 0
->
    [];
list_app_v1_by_label(Resource, Labels) when is_list(Resource) andalso is_map(Labels) ->
    LabelSelectorStr = uri_string:quote(to_label_selector(Labels)),
    Path = io_lib:format("/apis/apps/v1/~s?labelSelector=~s", [Resource, LabelSelectorStr]),
    ?LOG_DEBUG(#{method => "get", path => Path}),
    DeploymentList = kuberlnetes:get(Path, #{server => load_server_config()}),
    items(DeploymentList).

to_label_selector(Labels) ->
    S = maps:fold(
        fun(K, V, Acc) ->
            FmtStr = io_lib:format("~s=~s,", [K, V]),
            lists:merge(Acc, FmtStr)
        end,
        [],
        Labels
    ),
    %% remove the trailing comma
    string:trim(S, trailing, ",").

app_v1_resource_to_scale_target(
    #{
        <<"metadata">> := #{
            <<"name">> := Name,
            <<"namespace">> := Namespace,
            <<"annotations">> := Annotations
        },
        <<"spec">> := #{<<"replicas">> := CurrentReplicas}
    },
    Type
) ->
    DesiredReplicas = maps:get(?LAST_REPLICAS_ANNOTATION_KEY, Annotations, undefined),
    case Type of
        deployment ->
            aicore:deployment(Name, Namespace, CurrentReplicas, DesiredReplicas);
        statefulset ->
            aicore:stateful_set(Name, Namespace, CurrentReplicas, DesiredReplicas);
        _ ->
            ?LOG_ERROR(#{error => unsupported_resource_type, type => Type}),
            error(unsupported_type)
    end.

items(#{<<"items">> := Items}) -> Items.
