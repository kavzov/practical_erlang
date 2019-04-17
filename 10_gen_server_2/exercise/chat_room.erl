-module(chat_room).
-behavior(gen_server).

-export([start_link/0, add_user/3, remove_user/2, get_users/1, add_message/3, get_history/1, state/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([get_users_default/1, swap_list_tuples/1]).


%%% API
start_link() ->
    gen_server:start_link(?MODULE, [], []).

init([]) ->
    State = #{
        users    => [],
        messages => []
    },
    {ok, State}.

add_user(RoomPid, UserName, UserPid) ->
    gen_server:call(RoomPid, {add_user, UserName, UserPid}).

remove_user(RoomPid, UserPid) ->
    gen_server:call(RoomPid, {remove_user, UserPid}).

get_users(RoomPid) ->
    gen_server:call(RoomPid, get_users).

add_message(RoomPid, UserName, Msg) ->
    gen_server:call(RoomPid, {add_message, UserName, Msg}).

get_history(RoomPid) ->
    gen_server:call(RoomPid, get_history).

state(RoomPid) ->
    gen_server:call(RoomPid, state).


%%% Utils
get_users_default(Room) ->
    maps:get(users, Room).

get_messages(Room) ->
    maps:get(messages, Room).

user_in_room(UserPid, Users) ->
    proplists:is_defined(UserPid, Users).

swap_list_tuples(List) ->
    %% Swap places items in proplist tuples
    case is_proplist(List) of
        true -> 
            Res = lists:foldl(
                fun({K, V}, Acc) -> [{V, K} | Acc] end,
                [],
                List),
            {ok, Res};
        false ->
            {error, not_proplist}
    end.

is_proplist(List) ->
    lists:all(
        fun({_, _}) -> true;
           (Item) when is_atom(Item) -> true;
           (_) -> false
        end,
        List
    ).


%%% Handlers
handle_call({add_user, UserName, UserPid}, _From, State) ->
    Users = get_users_default(State),
    case user_in_room(UserPid, Users) of
        true  -> {reply, ok, State};
        false ->
            NewUser = {UserPid, UserName},
            NewState = State#{users => [NewUser | Users]},
            {reply, ok, NewState}
    end;

handle_call({remove_user, UserPid}, _From, State) ->
    Users = get_users_default(State),
    case user_in_room(UserPid, Users) of
        true -> 
            NewState = State#{users => proplists:delete(UserPid, Users)},
            {reply, ok, NewState};
        false ->
            {reply, {error, user_not_found}, State}
    end;

handle_call(get_users, _From, State) ->
    Users = get_users_default(State),
    {ok, Reply} = swap_list_tuples(Users),
    {reply, Reply, State};

handle_call({add_message, UserName, Msg}, _From, State) ->
    Messages = get_messages(State),
    Users = get_users_default(State),
    % send messages to users
    lists:foreach(
        fun({Pid, _}) -> chat_user:add_message(Pid, UserName, Msg) end,
        Users
    ),
    NewState = State#{messages => [{UserName, Msg} | Messages]},
    {reply, ok, NewState};

handle_call(get_history, _From, State) ->
    {reply, lists:reverse(get_messages(State)), State};

handle_call(state, _From, State) ->
    {reply, State, State};

handle_call(_Any, _From, State) ->
    {noreply, State}.


handle_cast(_Any, State) ->
    {noreply, State}.


handle_info(_Request, State) ->
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.


code_change(_OldVersion, State, _Extra) ->
    {ok, State}.
