%%%-------------------------------------------------------------------
%% @doc arbeitsinspektor top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(aicore_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 0,
        period => 1,
        %% usually auto shutdown should not be used on a top supervisor
        %% however for this application it is acually desired to
        %% terminate the top supervisor and the application if the
        %% worker terminates
        auto_shutdown => any_significant
    },
    ChildSpecs = [
        #{
            id => aicore_runner,
            start => {aicore_runner, start_link, []},
            restart => temporary,
            type => worker,
            significant => true
        }
    ],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
