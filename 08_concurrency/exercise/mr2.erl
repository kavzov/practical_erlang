-module(mr2).

-export([start/1, reducer/3, worker/3]).


start(Files) ->
    StartRef = make_ref(),
    spawn(?MODULE, reducer, [self(), StartRef, Files]),
    receive
        {StartRef, Res} -> Res
    after
        3000 ->
            io:format("No results from Reducer~n")
    end.


reducer(StartPid, StartRef, Files) ->
    WRefList = lists:map(
        fun(File) ->
            WRef = make_ref(),
            spawn(?MODULE, worker, [self(), WRef, File]),
            WRef
        end,
        Files
    ),
    Res = lists:foldl(
        fun(WRef, AccMap) ->
            receive
                {WRef, WordsCountMap} -> aggregate(AccMap, WordsCountMap)
            end
        end,
        #{},
        WRefList
    ),
    StartPid ! {StartRef, Res}.


worker(FromPid, WRef, File) ->
    Words = parse_file(File),
    FromPid ! {WRef, count_words(Words)}.


parse_file(File) ->
    case file:read_file(File) of
        {ok, Content} ->
            Words = binary:split(Content, [<<" ">>, <<"\n">>, <<"\r">>], [global]),
            lists:filter(fun(Word) -> Word =/= <<>> end, Words);
        {error, Reason} -> {error, Reason}
    end.


count_words(Words) ->
    lists:foldl(
        fun(Word, Acc) ->
            Acc#{Word => maps:get(Word, Acc, 0) + 1}
        end,
        #{},
        Words
    ).


aggregate(Map1, Map2) ->
    maps:fold(
        fun(K, V, Acc) ->
            case maps:find(K, Acc) of
                {ok, Value} -> Acc#{K := V + Value};
                error -> Acc#{K => V}
            end
        end,
        Map1,
        Map2
    ).
