-module(chat_user).
-behavior(gen_server).

-export([start_link/0, add_message/3, get_messages/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(msg, {
        user :: binary(),
        text :: binary()
    }
).


%%% API
start_link() ->
    gen_server:start_link(?MODULE, [], []).

init([]) ->
    State = [
        % {msg,"Bob","How are you doing, friend?"},{msg,"Bob","Hello from Bobby!"},{msg,"Helen","Hi man, I'm Helen!"}
    ],
    {ok, State}.

add_message(Pid, UserName, Message) ->
    gen_server:call(Pid, {add_message, #msg{user = UserName, text = Message}}).


get_messages(Pid) ->
    gen_server:call(Pid, get_messages).


%%% Handlers
handle_call({add_message, Message}, _From, State) ->
    {reply, ok, [Message | State]};

handle_call(get_messages, _From, State) ->
    Reply = lists:reverse(
        lists:map(
            fun(M) -> {M#msg.user, M#msg.text} end,
            State
        )
    ),
    {reply, Reply, State};

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
