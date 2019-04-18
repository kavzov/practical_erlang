-module(worker).

-export([parse_file/0]).


parse_file() ->
    receive
        {From, Ref, File} ->
            {ok, Data} = file:read_file(File),
            Records = binary:split(Data, [<<"\n">>, <<"\r">>], [global]),
            Records2 = lists:filter(fun(Item) -> Item =/= <<>> end, Records),
            Parsed_data = lists:foldl(
                fun(Rec, Acc) ->
                    [_, Name, Count, _] = binary:split(Rec, [<<",">>], [global]),
                    IntCount = list_to_integer(binary_to_list(Count)),
                    case maps:find(Name, Acc) of
                        {ok, CurrCount} -> Acc#{Name := CurrCount + IntCount};
                        error -> Acc#{Name => IntCount}
                    end
                end,
                #{},
                Records2
            ),
            From ! {reply, Ref, Parsed_data}
    after 3000 ->
        no_file_to_parse
    end.