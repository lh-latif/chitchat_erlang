-module(chitchat_user_chat_session).

-behaviour(gen_server).
-export([start_link/5,init/1]).
-export([handle_cast/2]).

start_link(UserSession,WsPid,ToChatSessPid,MyUsername,ToUsername) ->
    gen_server:start_link(
        chitchat_user_chat_session,
        [UserSession,WsPid,ToChatSessPid,MyUsername,ToUsername],
        []
    ).

init([UserSession,WsPid,ToChatSessPid,MyUsername,ToUsername]) ->
    %% seharusnya di user_session saat 'establish_chat_session'
    %% dicheck terlebih dahulu ke registry apakah toUsername exists.
    %% disini seharusnya tinggal mengecek ulang dan jika user_sessionnya mati
    %% maka tinggal digagalkan menghidupkan process dan akhirnya diatur
    %% oleh user_session untuk mengirimkan response failed/user_offline
    G = 7,
    N = randnum(),
    TryChatSessPid = case ToChatSessPid of
        nil ->
            get_to_user_chat_session(MyUsername, ToUsername);
        Pid ->
            {ok, Pid}
    end,
    case TryChatSessPid of
        {ok, ToChatSess_1} when ToChatSessPid == nil ->
            gen_server:cast(ToChatSess_1,{chat_session, share_public_key, {self(),G,N}}),
            gen_server:cast(self(),{chat_session, share_public_key, {ToChatSess_1,G,N}});
        _ ->
            nil
    end,
    case TryChatSessPid of
        {ok, ToChatSess} ->
            % io:format("user_chat: ~p ~p ~n", [TryChatSessPid, ToChatSess]),
            State = #{
                user_session => UserSession,
                ws_pid => WsPid,
                g => G,
                n => N,
                to_chat_sess => ToChatSess,
                my_username => MyUsername,
                to_username => ToUsername
            },
            {ok, State};
        {error, _} ->
            {error, errcannotup}
    end.

handle_cast({user_session, ws_new_pid, WsPid}, State) ->
    {noreply, maps:put(ws_pid, WsPid, State)};
handle_cast({user_session, send_chat, Content}, State) ->
    #{to_chat_sess := ChatSess} = State,
    gen_server:cast(
        ChatSess,
        {chat_session, receive_chat, Content}
    ),
    {noreply, State};
handle_cast({user_session, send_public_key, PublicKey}, State) ->
    #{to_chat_sess := ChatSess} = State,
    gen_server:cast(
        ChatSess,
        {chat_session, receive_shared_key, PublicKey}
    ),
    {noreply, State};
handle_cast({chat_session, share_public_key, {NewPid, G,N}}, State) ->
    #{ws_pid := WsPid, to_username := ToUsername} = State,
    erlang:send(WsPid, {chat_session, write_public_key, {ToUsername, G, N}}),
    {noreply, maps:put(to_chat_sess,NewPid,State)};

handle_cast({chat_session, receive_chat, Content}, State) ->
    #{ws_pid := WsPid, to_username := ToUsername} = State,
    erlang:send(WsPid, {chat_session, write_chat, {ToUsername, Content}}),
    {noreply, State};

handle_cast({chat_session, receive_shared_key, SharedKey}, State) ->
    #{ws_pid := WsPid, to_username := ToUsername} = State,
    erlang:send(WsPid, {chat_session, write_shared_key, {ToUsername, SharedKey}}),
    {noreply, State}.

get_to_user_chat_session(MyUsername, ToUsername) ->
    UserSession = gen_server:call(
        session_registry,
        {get_session, ToUsername}
    ),
    case UserSession of
        {just, Pid} ->
            ChatSession = gen_server:call(
                Pid,
                {chat_session, establish_chat_session, {MyUsername, self()}}
            ),
            {ok, ChatSession};
        none ->
            {error, errnousersess}
    end.

randnum() -> floor(rand:uniform() * 10).