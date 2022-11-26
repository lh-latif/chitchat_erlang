-module(chitchat_handler).

-export([init/2,upgrade/4]).
-include_lib("kernel/include/file.hrl").

just(Any) -> {just, Any}.

init(Req,State) ->
    {chitchat_handler, Req, State}.

upgrade(Req0,Env,_,_) ->
    #{method:=Method, path_info:=Path}
        = Req0,
    Route = {method(Method), Path},
    Req1 = open_routes(Route, Req0),
    Req2 = case Req1 of
        {next, Req_1} ->
            jwt_routes(
                Route, Req_1
            );
        Req_0 ->
            Req_0
    end,
    Req3 = case Req2 of
        {next, Req} ->
            cowboy_req:reply(
                404,
                #{},
                "not found",
                Req
            );
        Req ->
            Req
    end,
    {ok,Req3,Env#{result => ok}}.

open_routes(Route, Req) ->
    case Route of
        {get, [<<"app">>|_]} ->
            render_html(Req);
        {get, [<<"user">>,<<"index">>]} ->
            chitchat_user:index(Req);
        {post, [<<"user">>,<<"signin">>]} ->
            chitchat_user:user_signin(Req);
        {post, [<<"user">>,<<"register">>]} ->
            chitchat_user:user_register(Req);
        {get, [<<"debug">>,<<"users_session">>]} ->
            ListUser = gen_server:call(
                session_registry,
                {list_sessions, nil}
            ),
            cowboy_req:reply(
                200,#{},
                json:serialize(#{
                  <<"data">> => maps:keys(ListUser)    
                },
                #{return_binary => true}    
                ),
                Req
            );
        _Any ->
            {next, Req}
    end.

jwt_routes(Route, Req0) ->
    MaybeHandler = case Route of
        {get, [<<"user">>,<<"me">>]} ->
            just(fun(Req, User) -> 
                chitchat_user:me(Req, User)
            end);
        {get, [<<"user">>,<<"contact">>]} ->
            just(fun(Req,User) -> 
                chitchat_user_contact:index(Req, User)
            end);
        {post, [<<"user">>,<<"contact">>]} ->
            just(fun(Req,User) ->
                chitchat_user_contact:add(Req, User)
            end);
        {post, [<<"search_user">>]} ->
            just(fun(Req,_User) ->
                chitchat_search_user:search(Req)
            end);
        _ ->
            none
    end,
    Jwt = case MaybeHandler of
        none ->
            {error, Req0};
        _ ->
            chitchat_jwt:get_user_jwt(Req0)
    end,
    case {MaybeHandler,Jwt} of
        {none,_} ->
            {next, Req0};
        {{just, Handler1},
         {ok, User, Req1}
        } ->
            Handler1(Req1, User);
        {_Handler, {error, Req_1}} ->
            cowboy_req:reply(
                200, #{},
                <<"forbidden">>,
                Req_1
            )
    end.



method(<<"GET">>) -> get;
method(<<"POST">>) -> post;
method(<<"PATCH">>) -> patch;
method(<<"DELETE">>) -> delete;
method(<<"PUT">>) -> put.

render_html(Req) ->
    Path = template(<<"templates/app.html">>),
    % io:format("~p", [Path]),
    {ok, Info} = file:read_file_info(Path),

    cowboy_req:reply(
        200,
        #{<<"content-type">> => <<"text/html">>},
        {sendfile, 0, Info#file_info.size, Path},
        Req
    ).

template(File) ->
    case code:priv_dir(chitchat) of
        {error, _} -> crash;
        Dir ->
            filename:join(Dir, File)
    end.