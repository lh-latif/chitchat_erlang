-module(chitchat_search_user).

-export([search/1]).

search(Req) ->
    {ok, Body, Req1} = cowboy_req:read_body(Req),
    #{<<"username">> := Input
    } = jsone:decode(Body),
    Result = gen_server:call(
        sqlite_server,
        { execute, 
          [ <<"SELECT id, username FROM user ">>,
            <<"WHERE username LIKE '%">>,
            Input,<<"%';">>
          ]
        }
    ),
    cowboy_req:reply(
        200, #{<<"Content-Type">> => <<"application/json">>},
        jsone:encode(#{
            <<"data">> => chitchat_sqlite:format_result(Result)
        }), Req
    ).