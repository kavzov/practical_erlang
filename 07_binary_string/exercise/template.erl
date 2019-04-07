-module(template).

-export([parse/2]).

-define(StartTmpl, <<"{{">>).
-define(EndTmpl, <<"}}">>).
-define(EmptyStr, <<"">>).


get_tmpl_word(Tmpl) ->
    %% Extract {{key}} -> key
    binary:replace(
        binary:replace(Tmpl, ?EndTmpl, ?EmptyStr),
        ?StartTmpl, ?EmptyStr
    ).


to_binary(Value) ->
    %% Convert integer and string values to binary 
    IsInteger = is_integer(Value),
    IsList = is_list(Value),
    if 
        IsInteger -> integer_to_binary(Value);
        IsList -> list_to_binary(Value);
    true -> Value
    end.


parse(BinStr, Data) ->
    %% Parses a BinStr template values like {{template_word}} with the map Data values
    % Get start and end match indexes
    Starts = binary:matches(BinStr, ?StartTmpl),
    Ends = binary:matches(BinStr, ?EndTmpl),

    % Get list of {start, length} tuples to extract template words (for binary_part function)
    TmplIdxs = lists:map(fun(T)-> {A,B}=T, {A1,_}=A, {B1,B2}=B, {A1,B1+B2-A1} end, lists:zip(Starts, Ends)),

    % Get template words along with double parentheses ({{ }}) from BinStr. They will be replaced with its Data values.
    TmplWords = lists:map(
        fun(Idxs) -> binary_part(BinStr, Idxs) end,
        TmplIdxs
    ),

    % Replace the BinStr template words with its Data values
    lists:foldl(
        fun(Word, Str) -> 
            DataValue = to_binary(maps:get(get_tmpl_word(Word), Data, ?EmptyStr)),
            binary:replace(Str, Word, DataValue)
        end,
        BinStr,
        TmplWords
    ).

