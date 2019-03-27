-module(task_4).

-export([dropwhile/2, takewhile/2]).

-include_lib("eunit/include/eunit.hrl").


%% implement lists:dropwhile/2
%% http://www.erlang.org/doc/man/lists.html#dropwhile-2
% dropwhile(Pred, List) ->
%     List.
dropwhile(Pred, List) -> dropwhile(Pred, List, [], false).
dropwhile(_Pred, [], Acc, _Flag) -> lists:reverse(Acc);
dropwhile(Pred, [Elem | Rest], Acc, Flag) -> 
    Proper_elem = Pred(Elem),
    if
        Proper_elem -> dropwhile(Pred, Rest, Acc, true);
        true ->
            if
                Flag -> [Elem | Rest];
                true -> dropwhile(Pred, Rest, [Elem | Acc], false)
            end
    end.


dropwhile_test() ->
    F = fun(Val) -> Val =:= 32 end,
    ?assertEqual("hello", dropwhile(F, "   hello")),
    ?assertEqual([], dropwhile(F, [])),
    ?assertEqual([1,2,3], dropwhile(F, [1,2,3])),
    ?assertEqual([3,4], dropwhile(F, [32,3,4])),
    ?assertEqual([3,4], dropwhile(F, [32,32,3,4])),
    ?assertEqual([3,32,4,32], dropwhile(F, [32,32,32,32,32,32,3,32,4,32])),
    ok.


%% implement lists:takewhile/2
%% http://www.erlang.org/doc/man/lists.html#takewhile-2
% takewhile(Pred, List) ->
%     List.
takewhile(Pred, List) -> takewhile(Pred, List, [], [], false).

takewhile(_Pred, [], Acc, TmpAcc, _Flag) -> 
    if
        length(TmpAcc) > length(Acc) -> lists:reverse(TmpAcc);
        true -> lists:reverse(Acc)
    end;

takewhile(Pred, [Elem | Rest], Acc, TmpAcc, Flag) -> 
    Proper_elem = Pred(Elem),
    if
        Proper_elem -> takewhile(Pred, Rest, Acc, [Elem | TmpAcc], true);
        true -> 
            if
                Flag -> 
                    if 
                        length(TmpAcc) > length(Acc) -> takewhile(Pred, Rest, TmpAcc, [], false);
                        true -> takewhile(Pred, Rest, Acc, [], false)
                    end;
                true ->
                    takewhile(Pred, Rest, Acc, [], false)
            end
    end.


takewhile_test() ->
    F = fun(Val) -> Val =:= 32 end,
    ?assertEqual("   ", takewhile(F, "   hello")),
    ?assertEqual([], takewhile(F, [])),
    ?assertEqual([], takewhile(F, [1,2,3])),
    ?assertEqual([32], takewhile(F, [32,3,4])),
    ?assertEqual([32,32], takewhile(F, [32,32,3,4])),
    ?assertEqual([32,32,32,32,32,32], takewhile(F, [32,32,32,32,32,32,3,32,4,32])),
    F2 = fun(Val) -> Val < 5 end,
    ?assertEqual([1,2,3,4], takewhile(F2, [1,2,3,4,5,6,7,8])),
    ok.
