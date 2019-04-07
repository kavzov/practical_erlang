-module(short_link).

-export([init/0, create_short/2, get_long/2, rand_str/1]).

%%% module API

init() ->
    %% init randomizer
    <<A:32, B:32, C:32>> = crypto:strong_rand_bytes(12),
    rand:seed(exsp, {A,B,C}),
    State = #{},
    State.


create_short(LongLink, State) ->
    %% Creates new short link for LongLink or gets existing one if it already in the State
    %% Returns a tuple {NewShortLink, NewState} if a new short link have been created, {ExistingShortLink, ExistingState} otherwise
    BaseLink = "http://hexlet.io/",

    % Try to get LongLink from the State
    LinkMap = maps:filter(
        fun(_, V) -> LongLink == V end,
        State
    ),

    % If not - create new short link, put it into the State, otherwise get existing short link 
    if LinkMap == #{} ->
        ShortLink = BaseLink ++ rand_str(5),
        NewState = maps:put(ShortLink, LongLink, State),
        {ShortLink, NewState};
    true ->
        [Link] = maps:keys(LinkMap),
        {Link, State}
    end.


get_long(ShortLink, State) ->
    %% Gets LongLink by ShortLink
    %% Returns {ok, LongLink} if ShortLink is in the State, {error, not_found} otherwise
    LinkInState = maps:is_key(ShortLink, State),
    if
        LinkInState -> {ok, maps:get(ShortLink, State)};
        true -> {error, not_found}
    end.


%% generates random string of chars [a-zA-Z0-9]
rand_str(Length) ->
    lists:map(fun(Char) when Char > 83 -> Char + 13;
                 (Char) when Char > 57 -> Char + 7;
                 (Char) -> Char
              end,
              [rand:uniform(110 - 48) + 47 || _ <- lists:seq(1, Length)]).
