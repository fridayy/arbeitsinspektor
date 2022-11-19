%%%-------------------------------------------------------------------
%% @doc arbeitsinspektor public API
%% @end
%%%-------------------------------------------------------------------

-module(aicore_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    aicore_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
