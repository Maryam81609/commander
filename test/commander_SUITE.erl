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

-define(DEBUG, false).
-define(DEBUG_LOG(Arg), case ?DEBUG of
                            true -> ct:print(Arg);
                            false -> skip
                        end).

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

    SchedulerStr = os:getenv("SCHEDULER"),
    SchParamStr = os:getenv("SCHPARAM"),
    TestName = os:getenv("TEST"),
    CommDir = os:getenv("COMMDIR"),
    DefaultTestPath = os:getenv("TESTPATH"),
    DefaultSUTPath = os:getenv("SUTPATH"),
    DefaultBound = os:getenv("BOUND"),

    ConfigData = {commander, [
        {sut_app, list_to_atom(TestName)},
        {sut_dir, DefaultSUTPath},
        {test_dir, DefaultTestPath},
        {bound, list_to_integer(DefaultBound)}]},

    comm_config:init(ConfigData),

    TestDir = comm_config:get(test_dir),
    AppName = comm_config:get(sut_app),
    Bound = comm_config:get(bound),

    ?DEBUG_LOG("Read application env vars!"),
    ?DEBUG_LOG(io_lib:format("Test path: ~p", [TestDir])),
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

    true = code:add_path(TestDir),

    %% Set up Antidote DCs
    Clusters = test_utils:set_up_clusters_common(Config),
    test_utils:clocksi_check(Clusters),

    %% Setup SUT nodes
    SUTNodes = test_utils:start_sut_nodes(Clusters, Config),

    test_utils:set_test_node(Clusters),
    ?DEBUG_LOG("Passed nodes initialization!"),
    {ok, _} = application:ensure_all_started(commander),

    [{app, AppName}, {bound, Bound}, {comm_dir, CommDir}, {test_name, TestName},
        {sch_param, SchParam}, {scheduler, Scheduler}, {clusters, Clusters}, {sut_nodes, SUTNodes}|Config].

end_per_suite(Config) ->
    Config.

init_per_testcase(comm_check, Config) ->
    ct:timetrap({hours, 720}),
    Scheduler = proplists:get_value(scheduler, Config),
    DelayDirection = forward, %%backward,
    {ok, _Pid} = commander:start_link(Scheduler, DelayDirection),
    ?DEBUG_LOG(io_lib:format("Cammander started on: ~p", [node()])),
    ?DEBUG_LOG(io_lib:format("~p", [node()])),
    ok = commander:set_ct_config(Config),
    true = erlang:register(commander_booter, self()),
    Config.

end_per_testcase(comm_check, Config) ->
    ?DEBUG_LOG(io_lib:format("~p", [erlang:whereis(commander)])),
    Config.

comm_check(Config) ->
    {Bound, _} = string:to_integer(proplists:get_value(bound, Config)),
    SchParam = proplists:get_value(sch_param, Config),
    TestName = proplists:get_value(test_name, Config),
    TestModule = list_to_atom(TestName ++ "_comm"),
    AppName = proplists:get_value(app, Config),
    true = os:putenv("TESTNODE", atom_to_list(node())),

    %%% Check if commander process is running
    true = lists:member(commander, erlang:registered()),
    true = lists:member(commander_booter, erlang:registered()),
    ?DEBUG_LOG(io_lib:format("booter: ~p   vs.   self: ~p", [whereis(commander_booter), self()])),

    %%% Run the commander test case to record orig_exec
    ok = comm_utilities:write_to_file("comm_result",
        io_lib:format("~n===========================================================~n~n~w:~w~nParam:~p~n",
            [starting, erlang:localtime(), SchParam]), append),
    pass = TestModule:check(Config),

    Clusters = proplists:get_value(clusters, Config),
    ok = commander:get_clusters(Clusters),
    ?DEBUG_LOG(io_lib:format("Config: ~p", [Config])),
    %%% Do the exhaustive search
    commander:check(SchParam, Bound),

    %%% Receive stop message and teardown commander process
    receive
        stop ->
            ct:print("~p schedules replyed.", [commander:passed_test_count()]),
            ok = comm_utilities:write_to_file("comm_result",
                io_lib:format("~n~w:~w~n", [ending, erlang:localtime()]), append),
            SUTNodes = proplists:get_value(sut_nodes, Config),
            test_utils:stop_sut_nodes(SUTNodes, AppName),
            test_utils:wait_until_all_offline(SUTNodes),
            DBNodes = lists:flatten(Clusters),
            test_utils:stop_nodes(DBNodes),
            test_utils:wait_until_all_offline(DBNodes),
            commander:stop(),
            ct:print("Commander stoped on: ~p", [node()])
    end.