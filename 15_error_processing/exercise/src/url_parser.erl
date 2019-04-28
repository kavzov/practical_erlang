-module(url_parser).
-export([parse/1]).

-define(REOPTS, [{capture, all_names, binary}]).


-spec get_protocol(binary()) -> binary().
get_protocol(URL) ->
    RE = <<"^(?<PROTOCOL>http[s]?)://">>,
    case re:run(URL, RE, ?REOPTS) of
        {match, [Protocol]} -> Protocol;
        nomatch -> throw(invalidProtocol)
    end.

-spec get_domain(binary()) -> binary().
get_domain(URL) ->
    RE = <<"^http[s]?://(?<DOMAIN>((?!-)[\\w-]{1,63}(?<!-)\\.)+[a-zA-Z\\.?]{2,63})/.*">>,
    case re:run(URL, RE, ?REOPTS) of
        {match, [Domain]} -> Domain;
        nomatch -> throw(invalidDomain)
    end.

-spec get_path(binary()) -> binary().
get_path(URL) ->
    RE = <<"^http[s]?://[\\w\\.-]+/(?<PATH>.+)">>,
    case re:run(URL, RE, ?REOPTS) of
        {match, [Path]} -> Path;
        nomatch -> <<>>
    end.

-spec parse_path(binary()) -> [binary()].
parse_path(PathStr) ->
    [Path | _] = binary:split(PathStr, <<"?">>),
    lists:filter(
        fun(Item) -> Item =/= <<>> end,
        binary:split(Path, <<"/">>, [global])
    ).

-spec get_query(binary()) -> binary().
get_query(PathStr) ->
    [_ | Q] = binary:split(PathStr, <<"?">>),
    case Q of
        [] -> <<>>;
        [Query] -> Query
    end.

-spec get_date(binary()) -> {integer(), integer(), integer()} | undefined.
get_date(PathStr) ->
    RE = <<"^(\\d+)\/(0[1-9]|1[0-2])\/(0[1-9]|[1,2][0-9]|3[0,1])">>,
    case re:run(PathStr, RE, [{capture, all_but_first, binary}]) of
        {match, [Year, Month, Day]} ->
            {binary_to_integer(Year), binary_to_integer(Month), binary_to_integer(Day)};
        nomatch -> undefined
    end.


-spec parse(binary()) -> {ok, map()} | {error, term()}.
parse(URL) ->
    try
        Protocol = get_protocol(URL),
        Domain = get_domain(URL),
        PathStr = get_path(URL),
        Path = parse_path(PathStr),
        Query = get_query(PathStr),
        Date = get_date(PathStr),
        {ok, #{
                protocol => Protocol,
                domain => Domain,
                path => Path,
                query => Query,
                date => Date
        }}
    catch
        throw:invalidProtocol -> {error, invalid_protocol};
        throw:invalidDomain -> {error, invalid_domain}
    end.
