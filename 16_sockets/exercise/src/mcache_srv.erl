-module(mcache_srv).
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0, accept/2]).

-define(PORT, 1234).
-define(NUM_CONN, 3).
-define(SEP, <<" ">>).
-define(ENDSTR, <<"\r\n">>).
-define(BADREQ, <<"UNKNOWN REQUEST">>).


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


%% gen_server API
init([]) ->
    {ok, ListenSocket} = gen_tcp:listen(?PORT, [binary, {active, false}, {packet, line}, {reuseaddr, true}]),
    [spawn(?MODULE, accept, [Id, ListenSocket]) || Id <- lists:seq(1, ?NUM_CONN)],
    {ok, []}.

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


%% Internal functions
accept(Id, ListenSocket) ->
    io:format("Socket #~p is waiting for a connection~n", [Id]),
    case gen_tcp:accept(ListenSocket) of
        {ok, AcceptSocket} ->
            io:format("Socket #~p accepted the connection~n", [Id]),
            handle_connection(Id, ListenSocket, AcceptSocket);
        {error, Reason} ->
            io:format("Socket #~p error: \"~p\" ~n", [Id, Reason])
    end.

handle_connection(Id, ListenSocket, AcceptSocket) ->
    case gen_tcp:recv(AcceptSocket, 0) of
        {ok, ?ENDSTR} ->
            handle_connection(Id, ListenSocket, AcceptSocket);
        {ok, Msg} ->
            Reply = parse_msg(Msg),
            gen_tcp:send(AcceptSocket, <<Reply/binary, ?ENDSTR/binary>>),
            handle_connection(Id, ListenSocket, AcceptSocket);
        {error, Reason} ->
            io:format("Error: ~p~n", [Reason]),
            gen_tcp:close(AcceptSocket),
            accept(Id, ListenSocket)
    end.

parse_msg(Msg) ->
    [Command | ParamStr] = binary:split(msg_init_handle(Msg), ?SEP, [trim_all]),
    if ParamStr =/= [] ->
        case Command of
            <<"GET">> ->
                handle_1_param(ParamStr, fun mcache_storage:get_value/1);
            <<"DELETE">> ->
                handle_1_param(ParamStr, fun mcache_storage:delete_value/1);
            <<"SET">> ->
                handle_2_params(ParamStr, fun mcache_storage:set_value/2);
            <<"ADD">> ->
                handle_2_params(ParamStr, fun mcache_storage:add_value/2);
            <<"REPLACE">> ->
                handle_2_params(ParamStr, fun mcache_storage:replace_value/2);
            <<"APPEND">> ->
                handle_2_params(ParamStr, fun mcache_storage:append_value/2);
            <<"PREPEND">> ->
                handle_2_params(ParamStr, fun mcache_storage:prepend_value/2);
            <<"GETS">> ->
                [Params] = ParamStr,
                Keys = binary:split(Params, [?SEP, ?ENDSTR], [global, trim_all]),
                mcache_storage:get_values(Keys);
            _ ->
                ?BADREQ
        end;
        true ->
            ?BADREQ
    end.

msg_init_handle(Msg) ->
    % remove possible leading spaces
    Msg1 = re:replace(Msg, <<"^ +">>, <<>>, [{return, binary}]),
    % squeeze possible spaces sequences beetwen command and first param
    Msg2 = re:replace(Msg1, <<" +">>, <<" ">>, [{return, binary}]),
    % squeeze possible spaces sequences beetwen first and second param
    Msg3 = re:replace(Msg2, <<" +">>, <<" ">>, [{return, binary}]),
    % remove trailing line break
    re:replace(Msg3, <<?ENDSTR/binary, "$">>, <<>>, [{return, binary}]).

handle_1_param([Key], Fun) ->
    Key2 = re:replace(Key, <<" +$">>, <<>>, [{return, binary}]),  % remove possible trailing spaces
    Fun(Key2).

handle_2_params([Params], Fun) ->
    [Key | Value] = binary:split(Params, ?SEP, [trim_all]),
    case Value of
        [] -> ?BADREQ;
        _  -> Fun(Key, lists:nth(1, Value))
    end.
