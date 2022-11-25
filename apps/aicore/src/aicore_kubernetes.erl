%%%=============================================================================
%%% @doc aicore_kubernetes
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

%% @doc
%% Returns a list of all managed resources - meaning resources that contain 
%% arbeitsinspektor related annotations
%% @end
-spec business_hours_definitions() -> list(business_hour_defintion()).
business_hours_definitions() -> 
  BusinessHourDefinitions = kuberlnetes:get("/apis/arbeitsinspektor.ghcr.io/v1/businesshours?limit=1", #{server => load_server()}),
  %% there should only be only bhd object limited by resource quotas
  [#{<<"spec">> := #{<<"definitions">> := Definitions}} | _] = items(BusinessHourDefinitions),
  lists:map(fun(#{<<"definition">> := D, <<"label">> := L}) -> 
                MatchingDeployments = deployments(L),
                ManagedResources = lists:map(fun deployment_to_managed_resource/1, MatchingDeployments),
                #{business_hours => D, managed_resources => ManagedResources}
            end, Definitions).

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
  kuberlnetes:patch(#{path => Path, 
                      body => #{<<"spec">> => #{<<"replicas">> => ScaleTo}, <<"metadata">> => #{<<"annotations">> => #{<<"hui">> => <<"buh">>}}}}, 
                    #{server => load_server()}).

name(#{name := Name}) -> Name.
namespace(#{namespace := Namespace}) -> Namespace.

%% internal

%%TODO: configurable via application env and cache in ets table
load_server() ->
  kuberlnetes:from_config().


%% '/apis/apps/v1/deployments?labelSelector=env%3Ddevelopment&limit=500'
deployments(Labels) when map_size(Labels) =:= 0 -> [];
deployments(Labels) ->
  LabelSelectorStr = to_label_selector(Labels),
  Path = io_lib:format("/apis/apps/v1/deployments?labelSelector=~s", [LabelSelectorStr]),
  logger:debug("GET DeploymentList ('~s')", [Path]),
  DeploymentList = kuberlnetes:get(Path, #{server => load_server()}),
  items(DeploymentList).

to_label_selector(Labels) ->
  S = lists:foldl(fun({K, V}, Acc) -> 
                  FmtStr = io_lib:format("~s%3D~s,", [K,V]), %%url encode 'equals (=)'
                  lists:merge(Acc, FmtStr)
              end, [], maps:to_list(Labels)),
  string:trim(S, trailing, ","). %% remove the trailing comma

deployment_to_managed_resource(#{<<"metadata">> := #{<<"name">> := Name,
                                                     <<"namespace">> := Namespace},
                                 <<"spec">> := #{<<"replicas">> := Replicas}
                                }) ->
  #{name => Name, namespace => Namespace, current_replicas => Replicas}.

items(#{<<"items">> := Items}) -> Items.
