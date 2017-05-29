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

%% common_test callbacks
-export([
    init_per_suite/1,
    end_per_suite/1,
    all/0]).

-export([
    comm_check/1]).

all() -> [comm_check].

init_per_suite(Config) ->
    test_utils:at_init_testsuite(),
    Clusters = test_utils:set_up_clusters_common(Config),
    Nodes = lists:flatten(Clusters),

    if
        ?DEBUG -> ct:print("Clusters: ~p", [Clusters]);
        true -> skip
    end,

    %%% Ensure that the clocksi protocol is used
    test_utils:pmap(fun(Node) ->
        rpc:call(Node, application, set_env,
            [antidote, txn_prot, clocksi]) end, Nodes),

    %%% Check that indeed clocksi is running
    {ok, clocksi} = rpc:call(hd(hd(Clusters)), application, get_env, [antidote, txn_prot]),

    {ok, _} = application:ensure_all_started(commander),

    if
        ?DEBUG ->
            ct:print("Passed nodes initialization!")
    end,

    Scheduler = os:getenv("SCHEDULER"),
    SchParam = os:getenv("SCHPARAM"),

    %%% Read test name, dir and SUT dir from env
    TestPath = os:getenv("TESTPATH"),
    SUTPath = os:getenv("SUTPATH"),
    TestName = os:getenv("TEST"),

    if
        ?DEBUG ->
            ct:print("Read application env vars!"),
            ct:print("SUT path: ~p", [SUTPath]),
            ct:print("Test path: ~p", [TestPath]),
            ct:print("Scheduler: ~p", [Scheduler]),
            ct:print("SchParam: ~p", [SchParam])
    end,

    SUTPathAsList = string:tokens(SUTPath, "\/"),
    SUTName = lists:last(SUTPathAsList),
    SUTFullPath = filename:join([SUTPath, "_build", "default/lib", SUTName, "ebin"]),

    if
        ?DEBUG ->
            ct:print("before adding to code path!"),
            ct:print("Test path: ~p", [TestPath]),
            ct:print("SUT path: ~p", [SUTFullPath])
    end,

    %%% Add the test and SUT dirs to the code path
    true = code:add_path(SUTFullPath),
    true = code:add_path(TestPath),

    if
        ?DEBUG -> ct:print("Added to code path!")
    end,

    [{test_name, TestName}, {sch_param, SchParam}, {scheduler, Scheduler}, {clusters, Clusters}|Config]. %%

end_per_suite(Config) ->
    Config.

comm_check(Config) ->
    TestName = proplists:get_value(test_name, Config),
    TestModule = list_to_atom(TestName ++ "_comm"),
    TestModule:check(Config).