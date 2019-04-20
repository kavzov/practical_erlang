-module(mylib_worker).
-behavior(gen_server).

-export([start_link/0, get_version/0, get_modules/0, get_min_val/0, get_connection_timeout/0, all_apps/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(APP, mylib).


init([]) ->
    {ok, []}.


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


get_version() ->
    {ok, Version} = application:get_key(?APP, vsn),
    Version.

get_modules() ->
    {ok, Modules} = application:get_key(?APP, modules),
    Modules.

get_min_val() ->
    {ok, MinVal} = application:get_env(?APP, min_val),
    MinVal.

get_connection_timeout() ->
    {ok, ConnTimeout} = application:get_env(?APP, connection_timeout),
    ConnTimeout.

all_apps() ->
    Apps = application:which_applications(),
    lists:foldl(
        fun(App, Res) ->
            {Name, Descr, Version} = App,
            Res#{Name => #{description => Descr, version => Version}}
        end,
        #{},
        Apps
    ).


handle_call(_Any, _From, State) ->
    {noreply, State}.


handle_cast(_Any, State) ->
    {noreply, State}.


handle_info(_Request, State) ->
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.


code_change(_OldVersion, State, _Extra) ->
    {ok, State}.
