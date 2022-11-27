%%%=============================================================================
%%% @doc aicore_runner
%%% @end
%%%=============================================================================
-module(aicore_runner).

-behaviour(gen_server).

-export([init/1, handle_cast/2, handle_call/3, handle_info/2, terminate/2, handle_continue/2]).
-export([start_link/0]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_) ->
    {ok, none, {continue, run}}.

%% runs the cluster analysis
handle_continue(run, State) ->
    logger:info("Running cluster analysis"),
    aicore:analyze(),
    {noreply, State}.

%% unused
handle_cast(_, State) ->
    {noreply, State}.
handle_call(_, _, State) ->
    {reply, ok, State}.
handle_info(_, State) ->
    {noreply, State}.
terminate(_, _) ->
    ok.
