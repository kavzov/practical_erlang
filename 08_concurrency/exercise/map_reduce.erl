-module(map_reduce).

-export([start/1, get_stat/2, reduce/3]).


start(Files) ->
    %% Starts processes and receive handled data
    % Reduce process
    ReducePid = spawn(?MODULE, reduce, [#{}, length(Files), self()]),
    % Processes for the Files handle
    lists:foreach(
        fun(FileName) ->
            spawn(?MODULE, get_stat, [ReducePid, FileName])
        end,
        Files
    ),
    % Receive results from the reduce process
    receive
        {reply, Map} -> Map
    end.


get_stat(ReducePid, FileName) ->
    %% Handles file with Filename and send stat data to the reduce process
    % If a file exists, split its data by "\n" and " " symbols and collect statistics.
    case file:read_file(FileName) of
        {ok, Text} ->
            Words = binary:split(Text, [<<10>>, <<32>>], [global]),
            WordsMap = lists:foldl(
                fun(Word, Map) ->
                    case maps:find(Word, Map) of
                        {ok, Count} -> Map#{Word := Count + 1};
                        error   -> Map#{Word => 1}
                    end
                end,
                #{},
                Words
            ),
            % Send words statistics to the reduce process
            ReducePid ! {wordsmap, WordsMap};
        % Handle of non existing file
        {error, _Reason} ->
            io:format("File ~p not found~n", [FileName])
        end.


reduce(State, Iter, CallPid) ->
    %% Sumarizes a maps data from the received processes and send the result data to the call process
    % Iterates by number of files
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
                reduce(SumState, Iter-1, CallPid)
        after 1000 ->
            reduce(State, Iter-1, CallPid)
        end;
    % After iterations compliting send the reduced data to the call process
    true ->
        CallPid ! {reply, State}
    end.


