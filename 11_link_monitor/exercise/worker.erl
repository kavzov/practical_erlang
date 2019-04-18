-module(worker).

-export([parse_file/0]).


parse_file() ->
    receive
        {From, Ref, File} ->
            {ok, Data} = file:read_file(File),
            Items = binary:split(Data, [<<"\n">>, <<"\r">>], [global]),
            Items2 = lists:filter(fun(Item) -> Item =/= <<>> end, Items),
            Parsed = lists:foldl(
                fun(Item, Acc) ->
                    [_, Name, CountBin, _] = binary:split(Item, [<<",">>], [global]),
                    Count = list_to_integer(binary_to_list(CountBin)),
                    case maps:find(Name, Acc) of
                        {ok, CurCount} -> Acc#{Name := CurCount + Count};
                        error -> Acc#{Name => Count}
                    end
                end,
                #{},
                Items2
            ),
            From ! {reply, Ref, Parsed}
    after 5000 ->
        {error, no_incoming_data}
    end.