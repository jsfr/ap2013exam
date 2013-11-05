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

-module(at_extapi).

-export([abort/2, tryUpdate/2, ensureUpdate/2, choiceUpdate/3]).

%%%-------------------------------------------------------------------
%%% Extended API
%%%-------------------------------------------------------------------

abort(AT, Ref) ->
    at_server:update_t(AT, Ref, fun(_) -> exit(aborted) end),
    ok.

tryUpdate(AT, Fun) ->
    {ok, Ref} = at_server:begin_t(AT),
    case at_server:query_t(AT, Ref, fun(State) -> State end) of
        {ok, State} ->
            try Fun(State) of
                _ ->
                    at_server:update_t(AT, Ref, Fun),
                    case at_server:commit_t(AT, Ref) of
                        ok -> ok;
                        aborted -> aborted;
                        error -> aborted
                    end
            catch
                _:_ -> error
            end;
        aborted -> aborted;
        error -> aborted
    end.

ensureUpdate(AT, Fun) ->
    case tryUpdate(AT, Fun) of
        ok -> ok;
        aborted -> ensureUpdate(AT, Fun);
        error -> error
    end.

choiceUpdate(AT, Fun, Val_list) ->
    {ok, Ref} = at_server:begin_t(AT),
    case at_server:query_t(AT, Ref, fun(State) -> State end) of
        {ok, State} ->
            Self = self(),
            Length = length(Val_list),
            lists:map(fun(E) -> spawn(fun () -> evalFun(Self, Fun, State, Ref, E) end) end, Val_list),
            receiveUpdate(AT, Ref, Length, 0);
        aborted -> aborted;
        error -> error
    end.

receiveUpdate(AT, Ref, Length, Errors) ->
    receive
        {ok, {Ref, Fun, E}} ->
            at_server:update_t(AT, Ref, fun(State) -> Fun(State, E) end),
            case at_server:commit_t(AT, Ref) of
                ok -> {ok, E};
                aborted -> aborted;
                error -> error
            end;
        {error, Ref} ->
            NewErrors = Errors + 1,
            case NewErrors of
                Length -> error;
                _ -> receiveUpdate(AT, Ref, Length, NewErrors)
            end
    end.

evalFun(From, Fun, State, Ref, E) ->
    try Fun(State, E) of
        _ -> info(From, {ok, {Ref, Fun, E}})
    catch
        _:_ -> 
            info(From, {error, Ref})
    end.


%%%-------------------------------------------------------------------
%%% Communication primitives
%%%-------------------------------------------------------------------

info(Pid, Msg) ->
    Pid ! Msg.