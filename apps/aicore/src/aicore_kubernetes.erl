%%%=============================================================================
%%% @doc aicore_kubernetes
%%% @end
%%%=============================================================================
-module(aicore_kubernetes).

-export([]).

-record(managed_resource, {
          business_hour_definition,
          active_replicas
         }).

%% @doc
%% Returns a list of all managed resources - meaning resources that contain 
%% arbeitsinspektor related annotations
%% @end
-spec managed_resources() -> list().
managed_resources() -> [].

%% path: /apis/apps/v1/namespaces/{namespace}/deployments/{deploymentName}/scale
%% Request Body: {"spec":{"replicas":0}}
scale_to_zero(ManagedResource) ->
  ok.

