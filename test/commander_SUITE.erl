%%%-------------------------------------------------------------------
%%% @author maryam
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. May 2017 2:35 PM
%%%-------------------------------------------------------------------
-module(commander_SUITE).
-author("maryam").

-compile({parse_transform, lager_transform}).

-include_lib("common_test/include/ct.hrl").

-define(DEBUG, true).
-define(DEBUG_LOG(Arg), if ?DEBUG -> ct:print(Arg) end).

%% common_test callbacks
-export([
    init_per_suite/1,
    end_per_suite/1,
    init_per_testcase/2,
    end_per_testcase/2,
    all/0]).

-export([
    comm_check/1]).

all() -> [comm_check].

init_per_suite(Config) ->
    test_utils:at_init_testsuite(),
    true = erlang:set_cookie(node(), antidote),
    Clusters = test_utils:set_up_clusters_common(Config),
    Nodes = lists:flatten(Clusters),

    ?DEBUG_LOG(io_lib:format("Clusters: ~p", [Clusters])),

    %%% Send testing node name to all nodes
    test_utils:pmap(fun(Node) ->
        true = rpc:call(Node, os, putenv, ["TESTNODE", atom_to_list(node())])
        end, Nodes),

    %%% Ensure that the clocksi protocol is used
    test_utils:pmap(fun(Node) ->
        rpc:call(Node, application, set_env,
            [antidote, txn_prot, clocksi]) end, Nodes),

    %%% Check that indeed clocksi is running
    {ok, clocksi} = rpc:call(hd(hd(Clusters)), application, get_env, [antidote, txn_prot]),

    {ok, _} = application:ensure_all_started(commander),

    ?DEBUG_LOG("Passed nodes initialization!"),

    SchedulerStr = os:getenv("SCHEDULER"),
    SchParamStr = os:getenv("SCHPARAM"),
    TestPath = os:getenv("TESTPATH"),
    SUTPath = os:getenv("SUTPATH"),
    TestName = os:getenv("TEST"),

    ?DEBUG_LOG("Read application env vars!"),
    ?DEBUG_LOG(io_lib:format("SUT path: ~p", [SUTPath])),
    ?DEBUG_LOG(io_lib:format("Test path: ~p", [TestPath])),
    ?DEBUG_LOG(io_lib:format("Scheduler: ~p", [SchedulerStr])),
    ?DEBUG_LOG(io_lib:format("SchParam: ~p", [SchParamStr])),

    Scheduler =list_to_atom("comm_" ++ SchedulerStr ++ "_scheduler"),
    SchParam =
        case Scheduler of
            comm_random_scheduler ->
                {ok, S, _} = erl_scan:string(SchParamStr ++ "."),
                {ok, Seed} = erl_parse:parse_term(S),
                Seed;
            comm_delay_scheduler ->
                {DelayBound, _} = string:to_integer(SchParamStr),
                DelayBound
        end,
    DelayDirection = forward, %%backward,

    {ok, _Pid} = commander:start_link(Scheduler, DelayDirection),
    lager:info("Cammander started on: ~p", [node()]),
    ?DEBUG_LOG(io_lib:format("~p", [node()])),
    %%% Check if commander process is running
    true = lists:member(commander, erlang:registered()),

    ok = commander:set_test_node(node()),

    SUTPathAsList = string:tokens(SUTPath, "\/"),
    SUTName = lists:last(SUTPathAsList),
    SUTFullPath = filename:join([SUTPath, "_build", "default/lib", SUTName, "ebin"]),

    ?DEBUG_LOG("before adding to code path!"),
    ?DEBUG_LOG(io_lib:format("Test path: ~p", [TestPath])),
    ?DEBUG_LOG(io_lib:format("SUT path: ~p", [SUTFullPath])),

    %%% Add the test and SUT dirs to the code path
    true = code:add_path(SUTFullPath),
    true = code:add_path(TestPath),

    ?DEBUG_LOG("Added to code path!"),

    [{test_name, TestName}, {sch_param, SchParam}, {scheduler, Scheduler}, {clusters, Clusters}|Config].

end_per_suite(Config) ->
    Config.

init_per_testcase(comm_check, Config) ->
    Scheduler = proplists:get_value(scheduler, Config),
    DelayDirection = forward,
    {ok, _Pid} = commander:start_link(Scheduler, DelayDirection),
    lager:info("Cammander started on: ~p", [node()]),
    ?DEBUG_LOG(io_lib:format("~p", [node()])),
    Config.

end_per_testcase(comm_check, Config) ->
    ?DEBUG_LOG(io_lib:format("~p", [erlang:whereis(commander)])),
    Config.

comm_check(Config) ->
    TestName = proplists:get_value(test_name, Config),
    TestModule = list_to_atom(TestName ++ "_comm"),
    true = os:putenv("TESTNODE", atom_to_list(node())),
    %%% Check if commander process is running
    true = lists:member(commander, erlang:registered()),
    ?DEBUG_LOG(io_lib:format("~p", [erlang:registered()])),
    TestModule:check(Config).