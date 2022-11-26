-module(chitchat_user_session).

-behaviour(gen_server).
-define(chat_session, chitchat_user_chat_session).
-define(user_session, chitchat_user_session).
-export([start_link/2,init/1,handle_cast/2]).
-export([handle_call/3]).

start_link(WsPid,Username) ->
    gen_server:start(?user_session,[WsPid,Username],[]).

init([WsPid,Username]) ->
    State = #{
      ws_pid => WsPid,
      username => Username,
      chat_sessions => []
    },
    gen_server:call(
      session_registry,
      {register_session, {Username, self()}}
    ),
    {ok, State}.

handle_cast({ws, new_pid, WsPid}, State) ->
    broadcast_chat_sessions(State, {user_session, ws_new_pid, WsPid}),
    {noreply, maps:put(ws_pid, WsPid, State)};
handle_cast({establish_chat_session, Payload}, State) ->
    #{<<"username">> := ToUsername
    } = Payload,
    #{username := MyUsername,
      ws_pid := WsPid
    } = State,
    case find_chat_session(State, ToUsername) of
        none ->
            {ok, Pid} = ?chat_session:start_link(self(), WsPid, nil, MyUsername, ToUsername),
            #{chat_sessions := ChatSessions} = State,
            State1 = maps:put(
                chat_sessions,
                [{ToUsername, Pid}|ChatSessions],
                State
            ),
            {noreply, State1};
        {just, _} ->
            {noreply, State}
    end;
handle_cast({send_chat, Payload}, State) ->
    #{<<"username">> := ToUsername,
      <<"content">> := Content
    } = Payload,
    #{chat_sessions := ChatSessions} = State,
    ChatSession = find_chat_session(ChatSessions, ToUsername),
    case ChatSession of
        {just, ChatSession_1} ->
            gen_server:cast(
              ChatSession_1,
              {user_session, send_chat, Content}
            );
        none ->
            nil
    end,
    {noreply, State};
handle_cast({send_public_key, Payload}, State) ->
    #{<<"username">> := ToUsername,
      <<"public_key">> := PublicKey
    } = Payload,
    #{chat_sessions := ChatSessions} = State,
    ChatSession = find_chat_session(ChatSessions, ToUsername),
    case ChatSession of
        {just, ChatSession_1} ->
            gen_server:cast(
                ChatSession_1,
                {user_session, send_public_key, PublicKey}
            );
        none ->
            nil
    end,
    {noreply, State}.

handle_call({chat_session, establish_chat_session, {FromUsername, FromChatSess}}, _from, State) ->
    {ChatSession, State1} = case find_chat_session(State, FromUsername) of
        {just, Pid} ->
            {Pid, State};
        none ->
            #{username := MyUsername,
              ws_pid := WsPid
            } = State,
            {ok, Pid} = ?chat_session:start_link(self(), WsPid, FromChatSess, MyUsername, FromUsername),
            #{chat_sessions := ChatSessions} = State,
            State_1 = maps:put(
                chat_sessions,
                [{FromUsername, Pid}|ChatSessions],
                State
            ),
            {Pid, State_1}
    end,
    {reply, ChatSession, State1}.


find_chat_session([{Username,Pid}|_], Username) ->
    {just, Pid};
find_chat_session([_|Rest],Username) ->
    find_chat_session(Rest,Username);
find_chat_session([],_) ->
    none;
find_chat_session(#{chat_sessions := ChatSessions}, Username) ->
    find_chat_session(ChatSessions, Username).

broadcast_chat_sessions([{_Username,Pid}|Rest],Msg) ->
    erlang:send(Pid,Msg),
    broadcast_chat_sessions(Rest,Msg);
broadcast_chat_sessions([],_) ->
    [];
broadcast_chat_sessions(#{chat_sessions := ChatSessions}, Msg) ->
    broadcast_chat_sessions(ChatSessions, Msg).
