-module(sup_1).
-behaviour(supervisor).

-export([start_link/0, init/1, child_worker_spec/1]).
 

start_link() ->
    supervisor:start_link(?MODULE, []).


child_worker_spec(Id) ->
    #{
            id => Id,
            start => {worker, start_link, [Id]},
            restart => permanent,
            shutdown => 2000,
            type => worker,
            modules => [worker]
    }.


init(_Args) ->
    SupervisorSpecification =
    #{
        strategy => one_for_one,
        intensity => 10,
        period => 60
    },

    ChildSpecification = [
        child_worker_spec(worker_1),
        child_worker_spec(worker_2)
    ],

    {ok, {SupervisorSpecification, ChildSpecification}}.
