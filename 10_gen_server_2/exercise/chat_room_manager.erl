-module(chat_room_manager).
-behavior(gen_server).

-export([start_link/0,
         create_room/1, get_rooms/0,
         add_user/3, remove_user/2, get_users/1,
         send_message/3,  get_history/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


%%% API
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, []}.

create_room(Name) ->
    {ok, RoomPid} = chat_room:start_link(),
    gen_server:call(?MODULE, {create_room, Name, RoomPid}).

get_rooms() ->
    gen_server:call(?MODULE, get_rooms).


add_user(RoomPid, UserName, UserPid) ->
    gen_server:call(?MODULE, {add_user, RoomPid, UserName, UserPid}).

remove_user(RoomPid, UserPid) ->
    gen_server:call(?MODULE, {remove_user, RoomPid, UserPid}).

get_users(RoomPid) ->
    gen_server:call(?MODULE, {get_users, RoomPid}).


send_message(RoomPid, UserName, Msg) ->
    gen_server:call(?MODULE, {send_message, RoomPid, UserName, Msg}).

get_history(RoomPid) ->
    gen_server:call(?MODULE, {get_history, RoomPid}).


%%% Utils
is_room(RoomPid, Rooms) ->
    proplists:is_defined(RoomPid, Rooms).

user_in_room(UserPid, RoomPid) ->
    Users = chat_room:get_users(RoomPid),
    {ok, UsersPidName} = chat_room:swap_list_tuples(Users),
    proplists:is_defined(UserPid, UsersPidName).

room_not_found_reply() ->
    {error, room_not_found}.

%%% Handlers
handle_call({create_room, RoomName, RoomPid}, _From, State) ->
    NewState = [{RoomPid, RoomName} | State],
    Reply = {RoomName, RoomPid},
    {reply, Reply, NewState};

handle_call(get_rooms, _From, State) ->
    {ok, Reply} = chat_room:swap_list_tuples(State),
    {reply, Reply, State};

handle_call({add_user, RoomPid, UserName, UserPid}, _From, State) ->
    case is_room(RoomPid, State) of
        true ->
            Reply = chat_room:add_user(RoomPid, UserName, UserPid),
            {reply, Reply, State};
        false ->
            {reply, room_not_found_reply(), State}
    end;

handle_call({remove_user, RoomPid, UserPid}, _From, State) ->
    case is_room(RoomPid, State) of
        true ->
            case user_in_room(UserPid, RoomPid) of
                true ->
                    Reply = chat_room:remove_user(RoomPid, UserPid),
                    {reply, Reply, State};
                false ->
                    Reply = chat_room:user_not_found_reply(),
                    {reply, Reply, State}
            end;
        false ->
            {reply, room_not_found_reply(), State}
    end;

handle_call({get_users, RoomPid}, _From, State) ->
    case is_room(RoomPid, State) of
        true ->
            Users = chat_room:get_users(RoomPid),
            {reply, {ok, Users}, State};
        false ->
            {reply, room_not_found_reply(), State}
    end;

handle_call({send_message, RoomPid, UserName, Msg}, _From, State) ->
    case is_room(RoomPid, State) of
        true ->
            chat_room:add_message(RoomPid, UserName, Msg),
            {reply, ok, State};
        false ->
            {reply, room_not_found_reply(), State}
    end;

handle_call({get_history, RoomPid}, _From, State) ->
    case is_room(RoomPid, State) of
        true ->
            Messages = chat_room:get_history(RoomPid),
            {reply, {ok, Messages}, State};
        false ->
            {reply, room_not_found_reply(), State}
    end;

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
