-module(mcache_app).
-behavior(application).

-export([start/0, start/2, stop/1]).

start() ->
    application:start(mcache).

start(_Type, _Args) ->
    mcache_sup:start_link().

stop(_State) -> ok.