-module(chat_room_manager).

-export([start/0, stop/1, loop/1,
         create_room/2, remove_room/2, get_rooms/1, state/1,
         add_user/3, remove_user/3, get_users_list/2,
         send_message/4,  get_messages_history/2]).

-define(ROOMS_LIMIT, 5).
-define(ROOM, #{name => RoomName, users => [], messages => []}).


start() ->
    InitialState = #{
        % 1=>#{name=>"Rest",users=>["Helen","Bob"],messages=>[{"Helen","I like this room"},{"Helen","Will you come to us tonight?"},{"Bob","Hello rest room"},{"Bob","No. I'm busy tonight"}]},2=>#{name=>"Game",users=>["Kate","Bill"],messages=>[{"Bill","We don't talk anymore"},{"Bill","We will rock you!"},{"Kate","I love this Game room!"},{"Kate","I love my kitty too!"}]}
    },
    spawn(?MODULE, loop, [InitialState]).


%%%  API
call(Server, Msg) ->
    Ref = make_ref(),
    Server ! {self(), Ref, Msg},
    receive
        {reply, Ref, Reply} -> Reply
    after 3000 ->
        noreply
    end.

create_room(Server, RoomName) ->
    case valid_name(RoomName) of
        ok ->
            call(Server, {create_room, new_room_id(Server), ?ROOM});
        not_valid ->
            {error, not_valid_room_name}
    end.

remove_room(Server, RoomId) ->
    call(Server, {remove_room, RoomId}).

get_rooms(Server) ->
    call(Server, get_rooms).

state(Server) ->
    call(Server, state).

add_user(Server, RoomId, UserName) ->
    case valid_name(UserName) of
        ok ->
            call(Server, {add_user, RoomId, UserName});
        not_valid ->
            {error, not_valid_username}
    end.

remove_user(Server, RoomId, UserName) ->
    call(Server, {remove_user, RoomId, UserName}).

get_users_list(Server, RoomId) ->
    call(Server, {get_users_list, RoomId}).

send_message(Server, RoomId, UserName, Message) ->
    call(Server, {send_message, RoomId, UserName, Message}).

get_messages_history(Server, RoomId) ->
    call(Server, {get_messages_history, RoomId}).

stop(Pid) ->
    Pid ! stop.


%%%  Utils
valid_name(Name) ->
    RE = "^\\w+[\\w\\s-]*$",
    case re:run(Name, RE) of
        {match, _} -> ok;
        nomatch -> not_valid
    end.

new_room_id(Server) ->
    call(Server, get_last_room_id) + 1.


%%%  LOOP
loop(State) ->
    receive
        {From, Ref, Msg} ->
            {Res, NewState} = handle_call(State, Msg),
            From ! {reply, Ref, Res},
            ?MODULE:loop(NewState);

        stop ->
            io:format("~p stops by user~n", [self()]),
            ok;

        Msg ->
            io:format("ERROR: unknown message ~p~n", [Msg]),
            ?MODULE:loop(State)
    end.


%%%  CALLS HANDLERS
%%   ROOMS
handle_call(State, {create_room, RoomId, Room}) ->
    case maps:size(State) of
        ?ROOMS_LIMIT ->
            {{error, room_limit}, State};
        _ ->
            NewState = State#{RoomId => Room},
            {{ok, RoomId}, NewState}
    end;

handle_call(State, {remove_room, RoomId}) ->
    case maps:find(RoomId, State) of
        {ok, _Room} ->
            NewState = maps:remove(RoomId, State),
            {ok, NewState};
        error ->
            {{error, room_not_found}, State}
    end;

handle_call(State, get_rooms) ->
    Rooms = maps:map(
        fun(_RoomId, Room) -> maps:get(name, Room) end,
        State
    ),
    {maps:to_list(Rooms), State};


%%  USERS
handle_call(State, {add_user, RoomId, UserName}) ->
    case maps:find(RoomId, State) of
        {ok, Room} ->
            UsersInRoom = maps:get(users, Room),
            case lists:member(UserName, maps:get(users, Room)) of
                true ->
                    {{error, user_is_in_room}, State};
                false ->
                    NewState = State#{RoomId => Room#{users => [UserName | UsersInRoom]}},
                    {ok, NewState}
            end;
        error ->
            {{error, room_not_found}, State}
    end;

handle_call(State, {remove_user, RoomId, UserName}) ->
    case maps:find(RoomId, State) of
        {ok, Room} ->
            UsersInRoom = maps:get(users, Room),
            case lists:member(UserName, UsersInRoom) of
                true ->
                    NewState = State#{RoomId => Room#{users => lists:delete(UserName, UsersInRoom)} },
                    {ok, NewState};
                false ->
                    {{error, user_not_in_room}, State}
            end;
        error ->
            {{error, room_not_found}, State}
    end;

handle_call(State, {get_users_list, RoomId}) ->
    case maps:find(RoomId, State) of
        {ok, Room} ->
            {{ok, maps:get(users, Room)}, State};
        error ->
            {{error, room_not_found}, State}
    end;


%%  MESSAGES
handle_call(State, {send_message, RoomId, UserName, Message}) ->
    case maps:find(RoomId, State) of
        {ok, Room} ->
            UsersInRoom = maps:get(users, Room),
            case lists:member(UserName, UsersInRoom) of
                true ->
                    Messages = maps:get(messages, Room),
                    NewState = State#{RoomId => Room#{messages => [{UserName, Message} | Messages]}},
                    {ok, NewState};
                false ->
                    {{error, user_not_in_room}, State}
            end;
        error ->
            {{error, room_not_found}, State}
    end;

handle_call(State, {get_messages_history, RoomId}) ->
    case maps:find(RoomId, State) of
        {ok, Room} ->
            {{ok, maps:get(messages, Room)}, State};
        error ->
            {{error, room_not_found}, State}
    end;


%%  EXTRA
handle_call(State, get_last_room_id) ->
    Keys = lists:sort(maps:keys(State)),
    LastId = case Keys of
        [] -> 0;
        _ -> lists:last(Keys)
    end,
    {LastId, State};

handle_call(State, state) ->
    {State, State}.
