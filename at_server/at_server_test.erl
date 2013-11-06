-module(at_server_test).

-include_lib("eunit/include/eunit.hrl").

start_and_stop_server_test() ->
    {ok, Pid} = at_server:start(0),
    {ok, 0} = at_server:stop(Pid).

update_state_test() ->
    {ok, Pid} = at_server:start(0),
    {ok, Ref} = at_server:begin_t(Pid),
    {ok, Ref} = at_server:update_t(Pid, Ref, fun(X) -> X+1 end),
    {ok, 0} = at_server:doquery(Pid, fun(X) -> X end),
    ok = at_server:commit_t(Pid, Ref),
    {ok, 1} = at_server:stop(Pid).

aborted_commit_test() ->
    {ok, Pid} = at_server:start(0),
    {ok, Ref0} = at_server:begin_t(Pid),
    {ok, Ref1} = at_server:begin_t(Pid),
    {ok, 0} = at_server:query_t(Pid, Ref0, fun(X) -> X end),
    {ok, 0} = at_server:query_t(Pid, Ref1, fun(X) -> X end),
    {ok, Ref0} = at_server:update_t(Pid, Ref0, fun(X) -> X+1 end),
    ok = at_server:commit_t(Pid, Ref0),
    aborted = at_server:commit_t(Pid, Ref1),
    {ok, 1} = at_server:stop(Pid).

fail_on_query_test() ->
    {ok, Pid} = at_server:start(0),
    {ok, Ref} = at_server:begin_t(Pid),
    aborted = at_server:query_t(Pid, Ref, fun(_) -> exit(error) end),
    {ok, 0} = at_server:stop(Pid).

fail_on_update_test() ->
    {ok, Pid} = at_server:start(0),
    {ok, Ref} = at_server:begin_t(Pid),
    {ok, Ref} = at_server:update_t(Pid, Ref, fun(_) -> exit(error) end),
    timer:sleep(500),
    aborted = at_server:commit_t(Pid, Ref),
    {ok, 0} = at_server:stop(Pid).

abort_test() ->
    {ok, Pid} = at_server:start(0),
    {ok, Ref} = at_server:begin_t(Pid),
    ok = at_extapi:abort(Pid, Ref),
    aborted = at_server:commit_t(Pid, Ref),
    {ok, 0} = at_server:stop(Pid).

tryUpdate_test() ->
    {ok, Pid} = at_server:start(0),
    ok = at_extapi:tryUpdate(Pid, fun(X) -> X+1 end),
    {ok, 1} = at_server:doquery(Pid, fun(X) -> X end),
    error = at_extapi:tryUpdate(Pid, fun(_) -> exit(error) end),
    {ok, 1} = at_server:stop(Pid).

ensureUpdate_test() ->
    {ok, Pid} = at_server:start(0),
    spawn(fun() -> interfere(Pid) end),
    ok = at_extapi:ensureUpdate(Pid, fun(X) -> timer:sleep(1500), X+1 end),
    {ok, 2} = at_server:stop(Pid).

interfere(Pid) ->
    {ok, Ref} = at_server:begin_t(Pid),
    {ok, Ref} = at_server:update_t(Pid, Ref, fun(X) -> X+1 end),
    ok = at_server:commit_t(Pid, Ref).

choiceUpdate_test() ->
    {ok, Pid} = at_server:start(0),
    {ok, 1000} = at_extapi:choiceUpdate(Pid, fun(_, E) -> timer:sleep(E), E end, [1000, 5000, 10000]),
    {ok, 1000} = at_server:stop(Pid).