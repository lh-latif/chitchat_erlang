-module(chitchat_user_contact).

-export([index/2, add/2]).

index(Req, User) ->
    #{<<"id">> := UserId} = User,
    Result = gen_server:call(
        sqlite_server,
        { execute,
          [ <<"SELECT * FROM user_contact ">>,
            <<"WHERE user_id = ",(integer_to_binary(UserId))/binary,";">>
          ]
        }
    ),
    cowboy_req:reply(
        200, #{<<"Content-Type">> => "application/json"},
        json:serialize(#{
            "data" => chitchat_sqlite:format_result(Result)
        }, #{return_binary => true}),
        Req
    ).

add(Req, User) ->
    {ok, Body, Req1} = cowboy_req:read_body(Req),
    #{ <<"contact_id">> := ContactId,
       <<"contact_name">> := ContactName
    } = jsone:decode(Body),
    Result = gen_server:call(
        sqlite_server,
        { execute,
          [<<"SELECT * FROM user WHERE ">>,
           <<"id = ",(integer_to_binary(ContactId))/binary,";">>
          ]
        }
    ),
    [#{<<"id">> := UserContactId, <<"username">> := Username}] 
        = chitchat_sqlite:format_result(Result),
    #{<<"id">> := UserId} = User,
    Result1 = gen_server:call(
        sqlite_server,
        { execute,
          [<<"INSERT INTO user_contact (user_id, contact_id, contact_name, username) ">>,
           <<"VALUES (",(integer_to_binary(UserId))/binary,",'">>,
           integer_to_binary(UserContactId),<<"','">>,
           ContactName,<<"','",Username/binary,"');">>
          ]
        }
    ),
    % io:format("~p ~n", [Result1]),
    Req1.