-module(chitchat_websocket).

-export([init/2, websocket_handle/2, websocket_info/2]).

-define(wsproto, chitchat_wsproto).
-define(user_session, chitchat_user_session).
-define(session_registry, chitchat_session_registry).


init(Req,_State) ->
    State = #{
        user_session => nil
    },
    {cowboy_websocket, Req, State}.

websocket_handle(InFrame, State) ->
    {ok, Input} = chitchat_wsproto:parse_input(InFrame),
    #{<<"payload">> := Payload} = Input,
    #{user_session := UserSession} = State,
    HasSession = is_pid(UserSession),
    State1 = case chitchat_wsproto:get_command(Input) of
        none ->
            State;
        <<"establish_chat_session">> when HasSession ->
            gen_server:cast(UserSession,
              {establish_chat_session,
               Payload
              }
            ),
            State;
        <<"send_chat">> when HasSession ->
            gen_server:cast(UserSession,
              {send_chat,
               Payload
              }
            ),
            State;
        <<"send_public_key">> when HasSession ->
            gen_server:cast(UserSession,
              {send_public_key,
               Payload
              }
            ),
            State;
        <<"send_ready">> when HasSession ->
            State;
        <<"register">> when HasSession == false ->
            InitSession = case Payload of
                #{<<"username">> := Username} ->
                    {ok, get_or_init_session(Username)};
                _ ->
                    {error, errpayload}
            end,
            case InitSession of
                {ok, {just, UserSession_1}} when is_pid(UserSession_1) ->
                    #{user_session => UserSession_1};
                {error, _Err} ->
                    % io:format("error: ~p~n", [Err]),
                    State
            end

    end,
    % io:format("loop websocket: ~p~n~p~n",[State1, InFrame]),
    {ok, State1}.


websocket_info({chat_session, Cmd, Payload}, State) ->
    io:format("input: ~p~p~n", [Cmd,Payload]),
    Resp = case Cmd of
        write_chat ->
            {Username, Content} = Payload,
            [{text,
              serialize(#{
                <<"command">> => <<"receive_chat">>,
                <<"payload">> => #{
                    <<"content">> => Content,
                    <<"sender">> => Username
                }
              })
             }
            ];
        write_shared_key ->
            {Username, PublicKey} = Payload,
            % io:format("username: ~p~n", [Username]),
            [{text,
              serialize(#{
                <<"command">> => <<"receive_public_key">>,
                <<"payload">> => #{
                    <<"public_key">> => PublicKey,
                    <<"sender">> => Username
                }
              })
             }
            ];
        write_public_key ->
            {Username, G, N} = Payload,
            [{text,
              serialize(#{
                <<"command">> => <<"establish_chat_session">>,
                <<"payload">> => #{
                  <<"sender">> => Username,
                  <<"g">> => G,
                  <<"n">> => N
                }
              })
             }
            ];
        _ ->
            []
    end,
    {Resp, State}.


get_or_init_session(Username) ->
    Get = gen_server:call(
        session_registry,
        { get_session, Username
        }
    ),
    UserSession = case Get of
        none ->
            chitchat_user_session:start_link(self(),Username);
        {just, UserSession_1} ->
            gen_server:cast(UserSession_1,{ws, new_pid, self()}),
            {ok, UserSession_1}
    end,
    case UserSession of
        {ok, UserSession_2} ->
            {just, UserSession_2};
        _ ->
            none
    end.


serialize(Content) -> json:serialize(Content, #{return_binary => true}).