-module(map_reduce).

-export([start/1, get_stat/2, reduce/3]).


start(Files) ->
    ReducePid = spawn(?MODULE, reduce, [#{}, length(Files), self()]),
    lists:foreach(
        fun(FileName) ->
            spawn(?MODULE, get_stat, [ReducePid, FileName])
        end,
        Files
    ),
    receive
        {reply, Map} -> Map
    end.


get_stat(ReducePid, FileName) ->
    case file:read_file(FileName) of
        {ok, Text} ->
            Words = binary:split(Text, [<<10>>, <<32>>], [global]),
            WordsMap = lists:foldl(
                fun(Word, Map) ->
                    case maps:find(Word, Map) of
                        {ok, N} -> Map#{Word := N+1};
                        error   -> Map#{Word => 1}
                    end
                end,
                #{},
                Words
            ),
            ReducePid ! {wordsmap, WordsMap};
        {error, Reason} ->
            io:format("File ~p not found because of: ~p~n", [FileName, Reason])
        end.


reduce(State, Iter, ClientPid) ->
    if Iter > 0 ->
        receive
            {wordsmap, NewState} ->
                SumState = maps:fold(
                    fun(Word, Count, Acc) ->
                        case maps:find(Word, Acc) of
                            {ok, OldCount} -> Acc#{Word := Count + OldCount};
                            error -> Acc#{Word => Count}
                        end
                    end,
                    State,
                    NewState
                ),
                reduce(SumState, Iter-1, ClientPid)
        after 1000 ->
            reduce(State, Iter-1, ClientPid)
        end;
    true ->
        ClientPid ! {reply, State}
    end.


