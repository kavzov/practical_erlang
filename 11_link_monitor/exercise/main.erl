-module(main).

-export([parse/1]).


parse(Files) ->
    error_logger:tty(false),  % suppress = ERROR REPORT ====
    lists:foldl(
        fun(File, {Acc, Err}) ->
            Pid = spawn(worker, parse_file, []),
            Ref = erlang:monitor(process, Pid),
            Pid ! {self(), Ref, File},
            receive
                {reply, Ref, Reply} ->
                    erlang:demonitor(Ref, [flush]),
                    {aggregate(Acc, Reply), Err};
                {'DOWN', Ref, process, _Pid, Reason} ->
                    erlang:demonitor(Ref, [flush]),
                    {Acc, Err#{File => Reason}}
            after 5000 ->
                erlang:demonitor(Ref, [flush]),
                {error, no_reply}
            end
        end,
        {#{}, #{}},
        Files
    ).


aggregate(Map1, Map2) ->
    maps:fold(
        fun(K, V, Acc) ->
            case maps:find(K, Acc) of
                {ok, Val} -> Acc#{K => V + Val};
                error     -> Acc#{K => V}
            end
        end,
        Map1,
        Map2
    ).

