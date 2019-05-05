-module(mcache_storage).
-behaviour(gen_server).

-export([start_link/0, set_value/2, add_value/2, replace_value/2, append_value/2, prepend_value/2, get_value/1, get_values/1, delete_value/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(ENDSTR, <<"\r\n">>).
-define(STORED, <<"STORED">>).
-define(NOTFOUND, <<"NOT FOUND">>).
-define(END, <<"END">>).


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

set_value(Key, Val) ->
    gen_server:call(?MODULE, {set, Key, Val}).

add_value(Key, Val) ->
    gen_server:call(?MODULE, {add, Key, Val}).

replace_value(Key, Val) ->
    gen_server:call(?MODULE, {replace, Key, Val}).

append_value(Key, Val) ->
    gen_server:call(?MODULE, {append, Key, Val}).

prepend_value(Key, Val) ->
    gen_server:call(?MODULE, {prepend, Key, Val}).

get_value(Key) ->
    gen_server:call(?MODULE, {get, Key}).

get_values(Keys) ->
    gen_server:call(?MODULE, {gets, Keys}).

delete_value(Key) ->
    gen_server:call(?MODULE, {delete, Key}).


%%% gen_server API
init([]) ->
    Storage = ets:new(storage, []),
    {ok, Storage}.

handle_call({set, Key, Val}, _From, State) ->
    ets:insert(State, {Key, Val}),
    {reply, ?STORED, State};

handle_call({add, Key, Val}, _From, State) ->
    case ets:lookup(State, Key) of
        [_T] ->
            {reply, <<"EXISTS">>, State};
        [] ->
            ets:insert(State, {Key, Val}),
            {reply, ?STORED, State}
    end;

handle_call({replace, Key, Val}, _From, State) ->
    case ets:lookup(State, Key) of
        [_T] ->
            ets:insert(State, {Key, Val}),
            {reply, ?STORED, State};
        [] ->
            {reply, ?NOTFOUND, State}
    end;

handle_call({append, Key, Val}, _From, State) ->
    case ets:lookup(State, Key) of
        [{Key, OldVal}] ->
            NewVal = <<OldVal/binary, Val/binary>>,
            ets:insert(State, {Key, NewVal}),
            {reply, ?STORED, State};
        [] ->
            {reply, ?NOTFOUND, State}
    end;

handle_call({prepend, Key, Val}, _From, State) ->
    case ets:lookup(State, Key) of
        [{Key, OldVal}] ->
            NewVal = <<Val/binary, OldVal/binary>>,
            ets:insert(State, {Key, NewVal}),
            {reply, ?STORED, State};
        [] ->
            {reply, ?NOTFOUND, State}
    end;

handle_call({get, Key}, _From, State) ->
    Res = case ets:lookup(State, Key) of
        [{Key, Val}] ->
            <<"VALUE ", Key/binary, " ", Val/binary, ?ENDSTR/binary, ?END/binary>>;
        [] ->
            ?NOTFOUND
    end,
    {reply, Res, State};

handle_call({gets, Keys}, _From, State) ->
    Res = lists:foldl(
        fun(Key, Acc) ->
            Str = <<Acc/binary, "VALUE ", Key/binary, " ">>,
            case ets:lookup(State, Key) of
                [{Key, Val}] -> <<Str/binary, Val/binary, ?ENDSTR/binary>>;
                [] -> <<Str/binary, ?NOTFOUND/binary, ?ENDSTR/binary>>
            end
        end,
        <<>>,
        Keys
    ),
    {reply, <<Res/binary, ?END/binary>>, State};

handle_call({delete, Key}, _From, State) ->
    Res = case ets:lookup(State, Key) of
        [{Key, _}] ->
            ets:delete(State, Key),
            <<"DELETED">>;
        [] ->
            ?NOTFOUND
    end,            
    {reply, Res, State};

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
