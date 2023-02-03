%%%=============================================================================
%%% @doc aicore_runner
%%% @end
%%%=============================================================================
-module(aicore_runner).

-behaviour(gen_server).

-include_lib("kernel/include/logger.hrl").

-export([init/1, handle_cast/2, handle_call/3, handle_info/2, terminate/2, handle_continue/2]).
-export([start_link/0]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_) ->
    {ok, none, {continue, run}}.

handle_continue(run, State) ->
    ?LOG_INFO(#{event => running_inspection, message => "running cluster inspection"}),
    ok = aicore:perform_inspection_and_apply(),
    ?LOG_INFO(#{event => finished_inspection, message => "we are done here see you next time"}),
    % stop the vm in a controlled manner and exit with status code 0
    c:q(),
    {stop, normal, State}.

%% unused
handle_cast(_, State) ->
    {noreply, State}.
handle_call(_, _, State) ->
    {reply, ok, State}.
handle_info(_, State) ->
    {noreply, State}.
terminate(_, _) ->
    ok.
