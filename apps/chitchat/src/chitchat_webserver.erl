-module(chitchat_webserver).

-export([start_link/2, child_spec/0]).

start_link(_,_) -> 
    Dispatch = cowboy_router:compile([
        {'_', [{"/", hello_handler, []}]}
    ]),
    cowboy:start_clear(cowboy_httpserverx,
        [{port, 8000}],
        #{env => #{dispatch => Dispatch}}
    ).

child_spec() ->
    Dispatch = cowboy_router:compile([
        {'_', [ {<<"/ws">>, chitchat_websocket, []},
                {<<"/assets/[...]">>, cowboy_static, {dir, "./web_assets"}},
                {<<"/[...]">>, chitchat_handler, []}
              ]}
    ]),
    #{id => chitchat_httpserver,
      start => {cowboy, start_clear, 
        [cowboy_httpserverx, [{port, 8000}],
         #{env => #{dispatch => Dispatch}}
        ]
      },
      restart => permanent
    }.