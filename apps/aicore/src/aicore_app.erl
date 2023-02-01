%%%-------------------------------------------------------------------
%% @doc arbeitsinspektor public API
%% @end
%%%-------------------------------------------------------------------

-module(aicore_app).

-behaviour(application).

-include_lib("kernel/include/logger.hrl").

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    aicore_sup:start_link().

stop(State) ->
    ?LOG_INFO(#{event => stop, application => aicore, state => State}),
    ok.

%% internal functions
