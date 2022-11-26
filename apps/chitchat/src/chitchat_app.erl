%%%-------------------------------------------------------------------
%% @doc chitchat public API
%% @end
%%%-------------------------------------------------------------------

-module(chitchat_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    chitchat_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
