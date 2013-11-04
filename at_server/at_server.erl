%%%-------------------------------------------------------------------
%%% @author Michael Kirkedal Thomsen <shapper@diku.dk>
%%% @copyright (C) 2013, Michael Kirkedal Thomsen
%%% @doc
%%% Skeleton for AP Exam 2013.
%%% Implementation of the atomic transaction server
%%% @end
%%% Created : Oct 2013 by Michael Kirkedal Thomsen <shapper@diku.dk>
%%%-------------------------------------------------------------------
%%% Student name: Jens Fredskov
%%% Student KU-id: chw752
%%%-------------------------------------------------------------------

-module(at_server).

-export([start/1, stop/1, begin_t/1, doquery/2, query_t/3, update_t/3, commit_t/2]).

%%%-------------------------------------------------------------------
%%% API
%%%-------------------------------------------------------------------

start(State) ->
    {ok, spawn(fun () -> server_loop(State, dict:new()) end)}.

stop(AT) ->
    rpc(AT, stop).

doquery(AT, Fun) ->
    rpc(AT, {doquery, Fun}).

% Returns a reference
begin_t(AT) ->
    rpc(AT, begin_t).

query_t(AT, Ref, Fun) ->
    rpc(AT, {query_t, Ref, Fun}).

update_t(AT, Ref, Fun) ->
    info(AT, {update_t, Ref, Fun}),
    {ok, Ref}.

commit_t(AT, Ref) ->
    rpc(AT, {commit_t, Ref}).

%%%-------------------------------------------------------------------
%%% Communication primitives
%%%-------------------------------------------------------------------

%% synchronous communication

rpc(Pid, Request) ->
    Pid ! {self(), Request},
    receive
        {Pid, Response} -> Response
    end.

reply(From,  Msg) ->
    From ! {self(), Msg}.

reply_ok(From) ->
    reply(From, ok).

reply_ok(From, Msg) ->
    reply(From, {ok, Msg}).

reply_error(From) ->
    reply(From, error).

reply_abort(From) ->
    reply(From, aborted).

%% asynchronous communication

info(Pid, Msg) ->
    Pid ! Msg.

%%%-------------------------------------------------------------------
%%% Internal Implementation
%%%-------------------------------------------------------------------

server_loop(State, Helpers) ->
    receive
        {From, stop} ->
        dict:map(fun (_,V) -> abort_t(V) end, Helpers),
        reply_ok(From, State);

        {From, {doquery, Fun}} ->
        try
            Result = Fun(State),
            reply_ok(From, Result),
            server_loop(State, Helpers)
        catch
            _:_ ->
                reply_error(From),
                server_loop(State, Helpers)
        end;

        {From, begin_t} ->
        Ref = make_ref(),
        Helper = {ok, spawn(fun () -> transaction_helper(Ref, State) end)},
        NewHelpers = dict:store(Ref, Helper, Helpers),
        reply_ok(From, Ref),
        server_loop(State, NewHelpers);

        {From, {query_t, Ref, Fun}} ->
        case dict:find(Ref, Helpers) of
            {ok, {ok, Helper}} -> 
                case rpc(Helper, {query_t, Fun}) of
                    {ok, Result} ->
                        reply_ok(From, Result),
                        server_loop(State, Helpers);
                    aborted ->
                        NewHelpers = dict:store(Ref, {aborted, Helper}, Helpers), % Should we erase instead?
                        reply_abort(From),
                        server_loop(State, NewHelpers)
                end;
            {ok, {aborted, _}} -> % Should we erase here?
                reply_abort(From),
                server_loop(State, Helpers);
            error ->
                reply_error(From),
                server_loop(State, Helpers)
        end;

        {update_t, Ref, Fun} -> 
        case dict:find(Ref, Helpers) of
            {ok, {ok, Helper}} ->
                Self = self(),
                info(Helper, {Self, {update_t, Fun}}),
                server_loop(State, Helpers);
            {ok, {aborted, _}} ->
                server_loop(State, Helpers);
            error ->
                server_loop(State, Helpers)
        end;

        {From, {commit_t, Ref}} ->
        case dict:find(Ref, Helpers) of
            {ok, {ok, Helper}} ->
                NewHelpers0 = dict:erase(Ref, Helpers),
                NewHelpers1 = dict:map(fun (_,V) -> abort_t(V) end, NewHelpers0),
                {ok, NewState} = rpc(Helper, commit_t),
                reply_ok(From),
                server_loop(NewState, NewHelpers1);
            {ok, {aborted, _}} ->
                NewHelpers = dict:erase(Ref, Helpers),
                reply_abort(From),
                server_loop(State, NewHelpers);
            error ->
                reply_error(From),
                server_loop(State, Helpers)
        end;

        {aborted, Ref} -> % Helper informs when it aborts for some reason without us asking for it
        try
            NewHelpers = dict:update(Ref, fun({_, Helper}) -> {aborted, Helper} end, Helpers),
            server_loop(State, NewHelpers)
        catch
            _:_ -> server_loop(State, Helpers)
        end
    end.

abort_t({Atom, Pid}) ->
    case Atom of
        ok ->
            info(Pid, abort),
            {aborted, Pid};
        abort ->
            {aborted, Pid}
    end.

transaction_helper(Ref, State) ->
    receive
        abort ->
            ok;

        {From, {query_t, Fun}} ->
        try Fun(State) of
            Result ->
                reply_ok(From, Result),
                transaction_helper(Ref, State)
        catch
            _:_ -> reply_abort(From)
        end;

        {From, {update_t, Fun}} ->
        try Fun(State) of
            NewState ->
                transaction_helper(Ref, NewState)
        catch
            _:_ -> info(From, {aborted, Ref})
        end;

        {From, commit_t} ->
        reply_ok(From, State)
    end.