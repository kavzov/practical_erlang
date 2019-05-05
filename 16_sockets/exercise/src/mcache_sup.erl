-module(mcache_sup).
-behaviour(supervisor).
-export([start_link/0, init/1]).


start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).


init(_Args) ->
    SupervisorSpecification = #{
        strategy => one_for_one,
        intensity => 10,
        period => 60
    },
    ChildSpecification = [
        #{
            id => mcache_srv,
            start => {mcache_srv, start_link, []},
            restart => permanent,
            shutdown => 2000,
            type => worker,
            modules => [mcache_srv]
        },
        #{
            id => mcache_storage,
            start => {mcache_storage, start_link, []},
            restart => permanent,
            shutdown => 2000,
            type => worker,
            modules => [mcache_storage]
        }
    ],
    {ok, {SupervisorSpecification, ChildSpecification}}.
