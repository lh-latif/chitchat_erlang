-module(chitchat_sqlite).

-behaviour(gen_server).

-export([start_link/0, child_spec/0, init/1, handle_call/3]).
-export([format_result/1]).

start_link() ->
    % sqlite3:start_link(args1, opts).
    gen_server:start_link(
      {local, sqlite_server},
      chitchat_sqlite,
      [],
      []
    ).

init(_Opts) ->
    {ok, DB} = sqlite3:open(sqlite_app),
    migrate(DB),
    State = #{
        db => DB
        },
    {ok, State}.


child_spec() ->
    #{id => chitchat_sqlite,
    %   start => {sqlite3, start_link, [sqlite_app]}
      start => {chitchat_sqlite, start_link, []}
    }.


migrate(DB) ->
    sqlite3:create_table(
        DB, "user",
        [ {id, "INTEGER PRIMARY KEY AUTOINCREMENT"},
          {username, "STRING UNIQUE"},
          {hash, "string"},
          {created_at, "string"},
          {updated_at, "string"}
        ]
    ),
    sqlite3:create_table(
        DB, "user_contact",
        [ {id, "INTEGER PRIMARY KEY AUTOINCREMENT"},
          {user_id, "INTEGER"},
          {contact_id, "INTEGER"},
          {contact_name, "string"},
          {username, "STRING"},
          {created_at, "string"}
        ]
    ).


handle_call({execute, Sql}, _, State) ->
    #{db := DB} = State,
    Result = sqlite3:sql_exec(DB, Sql),    
    {reply, Result, State}.

format_result([{columns, ListColumns},{rows, Rows}]) ->
    parse_rows(Rows, ListColumns).

parse_rows([Row | Rows], Columns) ->
    [ maps:from_list(
        pair_column_row(Columns, tuple_to_list(Row))
      ) |
      parse_rows(Rows, Columns)
    ];
parse_rows([],_) ->
    [].

pair_column_row([C|CRest],[R|RRest]) ->
    [{binary:list_to_bin(C),R}|pair_column_row(CRest, RRest)];
pair_column_row([],[]) -> [].