-module(my_crypt).
-behaviour(gen_server).

-export([start_link/0, encode/1, get_key/0, set_key/1, hash/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
    crypt_key :: binary(),
    seed :: tuple()
}).


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

encode(Str) when is_binary(Str) ->
    gen_server:call(?MODULE, {encode, Str}).

get_key() ->
    gen_server:call(?MODULE, get_key).

set_key(Key) when is_binary(Key) ->
    gen_server:call(?MODULE, {set_key, Key}).

hash(Str) ->
    gen_server:call(?MODULE, {hash, Str}).


init([]) ->
    {ok, EncodeKey} = application:get_env(?MODULE, crypt_key),
    {ok, Seed} = application:get_env(?MODULE, seed),
    State = #state{
        crypt_key = EncodeKey,
        seed = Seed
    },
    {ok, State}.

handle_call({encode, BinStr}, _From, #state{crypt_key = EncodeKey} = State) ->
    Str = unicode:characters_to_list(BinStr),
    SH = unicode:characters_to_list(EncodeKey),
    EncodedStr = enc(Str, SH, SH, []),
    {reply, EncodedStr, State};

handle_call(get_key, _From, #state{crypt_key = EncodeKey} = State) ->
    {reply, EncodeKey, State};

handle_call({set_key, NewKey}, _From, State) ->
    NewState = State#state{crypt_key = NewKey},
    {reply, ok, NewState};

handle_call({hash, Str}, _From, #state{seed = Seed} = State) ->
    rand:seed(exsp, Seed),
    {ok, HashLength} = application:get_env(?MODULE, hash_size),
    T = [rand:uniform(255) || _<- lists:seq(1,255)],
    Hash = lists:foldl(
        fun(N, Acc) -> 
            [hash(T, Str, N) | Acc]
        end,
        [],
        lists:seq(1, HashLength)
    ),
   {reply, list_to_binary(Hash), State};


handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Request, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVersion, State, _Extra) ->
    {ok, State}.


enc([], _, _, Acc) -> unicode:characters_to_binary(lists:reverse(Acc));
enc(S, [], SH, Acc) -> enc(S, SH, SH, Acc);
enc([L|RestStr], [S|RestSh], SH, Acc) ->
    enc(RestStr, RestSh, SH, [L bxor S | Acc]).


hash(T, Str, N) ->
    H = byte_size(Str) rem 256,
    lists:foldl(
        fun(Elem, A) ->
            Idx = A bxor Elem bxor N,
            if Idx == 0 ->
                lists:nth(1, T);
            true ->
                lists:nth(Idx, T)
            end
        end,
        H,
        unicode:characters_to_list(Str)
    ).
