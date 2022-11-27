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

-export([business_hours_definitions/0, name/1, namespace/1, scale/3]).

%% represents a resource that is controlled by arbeitsinspektor
%% controlled in this context means that the resource has a label
%% that is defined in the business hours custom resource object.
-type managed_resource() :: #{
    %% the metadata.name of the resource
    name => binary(),
    %% the metadata.namespace of the resource
    namespace => binary(),
    %% current replicas
    current_replicas => non_neg_integer()
}.
-type business_hour_defintion() :: #{
    business_hours => binary(),
    managed_resources => list(managed_resource())
}.

-export_type([business_hour_defintion/0, managed_resource/0]).

-define(TABLE, aicore_kubenertes_server_config).

%% @doc
%% Returns a list of all managed resources - meaning resources that contain
%% arbeitsinspektor related annotations
%% @end
-spec business_hours_definitions() -> list(business_hour_defintion()).
business_hours_definitions() ->
    BusinessHourDefinitions = kuberlnetes:get(
        "/apis/arbeitsinspektor.ghcr.io/v1/businesshours?limit=1", #{server => load_server_config()}
    ),
    %% there should only be only bhd object limited by resource quotas
    [#{<<"spec">> := #{<<"definitions">> := Definitions}} | _] = items(BusinessHourDefinitions),
    lists:map(
        fun(#{<<"definition">> := D, <<"label">> := L}) ->
            MatchingDeployments = deployments(L),
            ManagedResources = lists:map(fun deployment_to_managed_resource/1, MatchingDeployments),
            #{business_hours => D, managed_resources => ManagedResources}
        end,
        Definitions
    ).

%% path: /apis/apps/v1/namespaces/{namespace}/deployments/{deploymentName}/scale
%% Request Body: {"spec":{"replicas":0}}
-spec scale(Name, Namespace, ScaleTo) -> ok | {error, Reason} when
    Name :: binary(),
    Namespace :: binary(),
    ScaleTo :: pos_integer(),
    Reason :: term().
scale(Name, Namespace, ScaleTo) when ScaleTo >= 0 ->
    Path = io_lib:format("/apis/apps/v1/namespaces/~s/deployments/~s/scale", [Namespace, Name]),
    logger:debug("PATCH Scale v1 ('~s')", [Path]),
    kuberlnetes:patch(
        #{
            path => Path,
            body => #{
                <<"spec">> => #{<<"replicas">> => ScaleTo},
                <<"metadata">> => #{<<"annotations">> => #{<<"hui">> => <<"buh">>}}
            }
        },
        #{server => load_server_config()}
    ).

name(#{name := Name}) -> Name.
namespace(#{namespace := Namespace}) -> Namespace.

%% internal
load_server_config() ->
    case ets:info(?TABLE) of
        undefined ->
            ets:new(?TABLE, [set, private, named_table]),
            ServerConfig = kuberlnetes:in_cluster(),
            ets:insert(?TABLE, {ServerConfig}),
            ServerConfig;
        _ ->
            ets:first(?TABLE)
    end.

%% '/apis/apps/v1/deployments?labelSelector=env%3Ddevelopment&limit=500'
deployments(Labels) when map_size(Labels) =:= 0 -> [];
deployments(Labels) ->
    LabelSelectorStr = to_label_selector(Labels),
    Path = io_lib:format("/apis/apps/v1/deployments?labelSelector=~s", [LabelSelectorStr]),
    logger:debug("GET DeploymentList ('~s')", [Path]),
    DeploymentList = kuberlnetes:get(Path, #{server => load_server_config()}),
    items(DeploymentList).

to_label_selector(Labels) ->
    S = lists:foldl(
        fun({K, V}, Acc) ->
            %%url encode 'equals (=)'
            FmtStr = io_lib:format("~s%3D~s,", [K, V]),
            lists:merge(Acc, FmtStr)
        end,
        [],
        maps:to_list(Labels)
    ),
    %% remove the trailing comma
    string:trim(S, trailing, ",").

deployment_to_managed_resource(#{
    <<"metadata">> := #{
        <<"name">> := Name,
        <<"namespace">> := Namespace
    },
    <<"spec">> := #{<<"replicas">> := Replicas}
}) ->
    #{name => Name, namespace => Namespace, current_replicas => Replicas}.

items(#{<<"items">> := Items}) -> Items.
