-module(chitchat_wsproto).

-export([parse_input/1,get_command/1]).

parse_input({text, Input}) ->
    json:parse(
        Input
    ).

get_command(#{<<"command">> := Command}) -> Command;
get_command(_) -> none.
