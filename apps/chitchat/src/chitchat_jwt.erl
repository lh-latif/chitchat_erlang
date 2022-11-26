-module(chitchat_jwt).

-export([start_link/0, child_spec/0, init/1, handle_call/3, get_user_jwt/1]).

start_link() ->
    gen_server:start_link(
        {local, jwt_server},
        chitchat_jwt,
        [],
        []
    ).

child_spec() ->
    #{ id => chitchat_jwt,
       start => {chitchat_jwt, start_link, []}
    }.

init(_) ->
    State = #{
        jwk => #{
            <<"kty">> => <<"oct">>,
            <<"k">> => jose_base64url:encode("Ini Secret Key")
        },
        jws => #{
            <<"alg">> => <<"HS512">>
        },
        alg => #{alg => jose_jws_alg_hmac}
    },
    {ok, State}.

handle_call({sign, Payload}, _From, State) ->
    Token = sign(Payload, State),
    {reply, Token, State};

handle_call({verify, Token}, _From, State) ->
    Payload = verify(Token,State),
    {reply, Payload, State}.

sign(Payload, #{jws := Jws, jwk := Jwk}) ->
    Signed = jose_jwt:sign(Jwk, Jws, Payload),
    {_, Token} = jose_jws:compact(Signed),
    Token.

verify(Token, #{alg := Alg, jwk := Jwk}) ->
    {true, Jwt, Jws} =jose_jwt:verify(Jwk, {Alg, Token}),
    {Jwt, Jws}.

get_user_jwt(Req) ->
    #{headers := Headers} = Req,
    Token = case Headers of
        #{<<"authorization">> :=
            <<"Bearer ",Token0/binary>>
        } when size(Token0) > 10 ->
            {just, Token0};
        _ ->
            none
    end,
    Jwt = case Token of
        {just, Token1} ->
            gen_server:call(jwt_server,
              {verify, Token1}
            );
        _ ->
            none
    end,
    Act2 = case Jwt of
        none ->
            none;
        {{jose_jwt, #{
              <<"username">> := Username
         }},
         _jose_jwk
        } ->
            gen_server:call(
                sqlite_server,
                {execute,
                 [<<"SELECT * FROM user WHERE username=\"",Username/binary,"\";">>]
                }
            )
    end,
    User = case Act2 of
        Data when is_list(Data) ->
            {just, chitchat_sqlite:format_result(Data)};
        _ ->
            none
    end,
    case User of
        {just, [User1]} ->
            {ok, User1, Req};
        none ->
            {error, Req}
    end.