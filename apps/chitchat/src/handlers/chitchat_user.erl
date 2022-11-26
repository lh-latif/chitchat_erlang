-module(chitchat_user).

-export([user_signin/1,user_register/1,index/1,me/2]).


user_signin(Req) ->
    {ok, Body, Req1} = cowboy_req:read_body(Req),
    #{<<"username">> := Username,
      <<"password">> := Password       
    } = jsone:decode(Body),
    Result = gen_server:call(
        sqlite_server,
        { execute,
          [<<"SELECT * FROM `user` WHERE username=\"">>,Username,<<"\";">>]
        }
    ),
    [#{<<"hash">> := Hash}] = chitchat_sqlite:format_result(Result),
    CheckPw = argon2:verify_with_secret(Password, Hash, <<"ini password secret">>),
    JwtResult = case CheckPw of
        {ok, _} ->
            Token = gen_server:call(
                jwt_server,
                {sign, #{
                    <<"username">> => Username
                }}
            ),
            {ok, Token};
        _ ->
            err
    end,
    case JwtResult of
        {ok, Token1} ->
            cowboy_req:reply(
                200,#{"content-type" => "application/json"},
                jsone:encode(#{
                    <<"token">> => Token1
                }),
                Req1
            );
        _ ->
            cowboy_req:reply(
                403,#{},
                <<"">>,
                Req1
            )
    end.

user_register(Req) ->
    {ok, Body, Req1} = cowboy_req:read_body(Req),
    #{<<"username">> := Username,
      <<"password">> := Password       
    } = jsone:decode(Body),
    {ok, Hash} = argon2:hash_with_secret(
        Password, <<"ini password secret">>
    ),
    Result = gen_server:call(
        sqlite_server,
        { execute,
          [<<"INSERT INTO user (username, hash) VALUES (\"">>,Username,<<"\",\"">>,Hash,<<"\");">>]
        }
    ),
    Req1.

index(Req) ->
    Result = gen_server:call(
        sqlite_server,
        { execute,
          [<<"SELECT * FROM USER;">>]
        }
    ),
    Data = chitchat_sqlite:format_result(Result),
    % io:format("~p~n", [Data]),
    cowboy_req:reply(
        200,#{},jsone:encode(#{
            <<"data">> => Data
        }),
        Req
    ).

me(Req, User) ->
    % io:format("~p ~n",[User]),
    #{<<"username">> := Username
    } = User,
    cowboy_req:reply(
        200, #{<<"content-type">> => <<"json">>},
        jsone:encode(#{
            <<"data">> => #{
                <<"username">> => Username
            } 
        }),
        Req
    ).