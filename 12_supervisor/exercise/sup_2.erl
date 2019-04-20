-module(sup_2).
-behaviour(supervisor).

-export([start_link/0, init/1, add_worker/1, remove_worker/1]).


start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_Args) ->
    SupervisorSpecification =
    #{
        strategy => one_for_one,
        intensity => 10,
        period => 60
    },

    ChildSpecification = [
        sup_1:child_worker_spec(worker_3),
        sup_1:child_worker_spec(worker_4)
    ],

    {ok, {SupervisorSpecification, ChildSpecification}}.


add_worker(Id) ->
    supervisor:start_child(?MODULE, sup_1:child_worker_spec(Id)).

remove_worker(Id) ->
    case supervisor:terminate_child(?MODULE, Id) of
        ok -> supervisor:delete_child(?MODULE, Id);
        {error,not_found} -> {error, worker_not_found}
    end.
