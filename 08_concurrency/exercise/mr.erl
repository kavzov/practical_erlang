-module(mr).

-export([start/0, start/1, reducer/3, parse_file/1, worker/3]).


start() ->
    start(["data1.txt", "data3.txt", "data3.txt", "data4.txt", "data5.txt"]).

% start(Files) ->
%     Maps = lists:map(fun(File) -> parse_file(File) end, Files),
%     lists:foldl(
%         fun(Map, Acc) -> aggregate(Map, Acc) end,
%         #{},
%         Maps
%     ).

start(Files) ->
    io:format("start ~p~n", [self()]),
    Ref = make_ref(),
    spawn(?MODULE, reducer, [self(), Ref, Files]),
    receive
        {Ref, Res} -> {ok, Res}
    after 3000 ->
        {error, no_reply}
    end.


reducer(FromPid, Ref, Files) ->
    io:format("reducer ~p~n", [self()]),
    % Maps = lists:map(fun(File) -> parse_file(File) end, Files),
    % Res = lists:foldl(
    %     fun(Map, Acc) -> aggregate(Map, Acc) end,
    %     #{},
    %     Maps
    % ),
    Wait = lists:map(
        fun(File) ->
            WRef = make_ref(),
            WPid = spawn(?MODULE, worker, [self(), WRef, File]),
            {WRef, WPid}
        end,
        Files
    ),
    Res = loop(Wait, #{}),
    FromPid ! {Ref, Res}.
    % io:format("Wait: ~p~n", [Wait]).
    % FromPid ! {Ref, Res}.

loop([], Acc) -> Acc;
loop([{Ref, _} | Wait], Acc) ->
    receive{Ref, Res} ->
        loop(Wait, aggregate(Res, Acc))
    after
        5000 ->
            io:format("Error: no reply from worlers, ~p~p~n", [Ref, Wait])
    end.



worker(ReducerPid, Ref, File) ->
    io:format("worker ~p~n", [self()]),
    Res = parse_file(File),
    ReducerPid ! {Ref, Res}.


parse_file(File) ->
    {ok, Content} = file:read_file(File),
    Words = binary:split(Content, [<<" ">>, <<"\n">>, <<"\r">>], [global]),
    Words2 = lists:filter(fun(Word) -> Word /= <<>>end, Words),
    Words,
    lists:foldl(
        fun(Word, Acc) ->
            Acc#{Word => maps:get(Word, Acc, 0) + 1}
        end,
        #{},
        Words2
    ).

aggregate(Map1, Map2) ->
    maps:fold(
        fun(K, V, Acc) ->
            case maps:find(K, Acc) of
                {ok, Val} -> Acc#{K := V+Val};
                error -> Acc#{K => V}
            end
        end,
        Map1,
        Map2
    ).