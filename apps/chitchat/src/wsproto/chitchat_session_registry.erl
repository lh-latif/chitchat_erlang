-module(chitchat_session_registry).

-behaviour(gen_server).

-export([start_link/0,init/1,child_spec/0,handle_call/3]).


start_link() ->
    gen_server:start_link(
        {local, session_registry},
        chitchat_session_registry,
        [],
        []
    ).

child_spec() ->
    #{id => chitchat_session_registry,
      start => {chitchat_session_registry, start_link, []}
    }.

init(_) ->
    State = #{
        registry => #{}
    },
    {ok, State}.
handle_call({get_session, Username}, _From, State) ->
    #{registry := Registry} = State,
    Session = case maps:get(Username, Registry, none) of
        none -> none;
        Session_1 -> {just, Session_1}
    end,
    {reply, Session, State};
handle_call({list_sessions, _}, _Form, State) ->
    #{registry := Registry} = State,
    {reply, Registry, State};
handle_call({register_session, {Username, Pid}}, _From,  State) ->
    #{registry := Registry} = State,
    Registry1 = maps:put(Username, Pid, Registry),
    {reply, ok, maps:put(registry, Registry1, State)}.


find_session([{Username, Pid}| _Rest], Username) ->
    {just, Pid};
find_session([_ | Rest], Username) ->
    find_session(Rest, Username);
find_session([],_) ->
    none.