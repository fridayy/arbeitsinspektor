-record(business_hour_definition, {
          %% specification as string e.g: mon-tue[10:00-19:00]
          spec :: string(),
          labels :: map()
         }).

-record(scale_target, {
          name :: string(),
          namespace :: string(),
          %% the current set spec.replicas
          current_replicas :: number(),
          %% desired replicas is read from the "arbeitsinspector.ghcr.io/last-replicas" annotations
          desired_replicas :: number() | undefined,
          kind :: deployment | statefulset
         }).

-record(scale_targets, {
          targets :: list(#scale_target{}),
          business_hour_definition :: #business_hour_definition{}
         }).
