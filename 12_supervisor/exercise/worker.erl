-module(worker).
-behaviour(gen_server).

-export([start_link/1, ping/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


init([Id]) ->
    {ok, Id}.


start_link(Id) ->
    gen_server:start_link(?MODULE, [Id], []).


ping(Pid) ->
    gen_server:call(Pid, {ping, Pid}).


handle_call({ping, Pid}, _From, State) ->
    {reply, {State, Pid}, State};

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
