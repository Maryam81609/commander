%%%-------------------------------------------------------------------
%%% @author maryam
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. May 2017 11:37 AM
%%%-------------------------------------------------------------------
-module(wallet_comm).
-author("maryam").

-behavior(comm_test).

-include_lib("eunit/include/eunit.hrl").

%% comm_test callbacks
-export([check/1, handle_event/1, handle_object_invariant/2]).

-define(INIT_VAL, 26500).

%% TODO: allow the programmer to specify
%% the number of clusters and the number
%% of nodes in each
check(Config) ->
    Clusters = proplists:get_value(clusters, Config),
    [Node1, Node2, Node3 | _Nodes] =  [ hd(Cluster)|| Cluster <- Clusters ],

    main_test(Node1, Node2, Node3).

main_test(Node1, Node2, Node3) ->
    Key = wallet_key,
    Wallet = {Key, antidote_crdt_counter, bucket},

    TestNode = list_to_atom(os:getenv("TESTNODE")),
    true = rpc:call(TestNode, lists, member, [commander, erlang:registered()]),

    %%% Specify invariant objects
    comm_test:objects(?MODULE, [Wallet]),

    {_Re, CT} = comm_test:event(?MODULE, [2, Node1, ignore, [Wallet, 26500]]),

    ?DEBUG_LOG(io_lib:format("~n^^^^^^^^^^CT: ~w^^^^^^^^^^^~n", [dict:to_list(CT)])),
    [?DEBUG_LOG(io_lib:format("Wallet Val on node: ~p ==> ~p", [Node, wallet:get_val(Node, Wallet, CT)]))
        || Node <- [Node1, Node2, Node3]],

    [?assertEqual(wallet:get_val(Node, Wallet, CT), 26500) || Node <- [Node1, Node2, Node3]],

    CT1 = dc1_txns(Node1, Wallet, CT),
    CT2 = dc2_txns(Node2, Wallet, CT),
    CT3 = dc3_txns(Node3, Wallet, CT),

    Time = dict:merge(fun(_K, T1, T2) -> max(T1, T2) end,
        CT1,
        dict:merge(fun(_K, T1, T2) -> max(T1, T2) end, CT2, CT3)),

    Vals = [wallet:get_val(Node, Wallet, Time) || Node <- [Node1, Node2, Node3]],
    ct:print("Vals: ~w", [Vals]),

    ?DEBUG_LOG(io_lib:format("CT1:~w~nCT2:~w~nCT3:~w~n~nTime:~w~n",
                [dict:to_list(CT1), dict:to_list(CT2), dict:to_list(CT3), dict:to_list(Time)])),
    ?DEBUG_LOG(io_lib:format("Val on node 1: ~p", [wallet:get_val(Node1, Wallet, Time)])),
    Quiescence_val = lists:usort(Vals),
    ?assertMatch(Quiescence_val, [hd(Vals)]),
    ?DEBUG_LOG(io_lib:format("Cookie: ~p, Node: ~p", [erlang:get_cookie(), node()])),
    ?DEBUG_LOG(io_lib:format("comm test Self: ~p   commander: ~p", [self(), whereis(commander)])),

    pass.

dc1_txns(Node, Wallet, ST) ->
    {_R1, _CT1} = comm_test:event(?MODULE, [1, Node, ST, [Wallet, 200]]),
    {_R2, _CT2} = comm_test:event(?MODULE, [1, Node, ST, [Wallet, 300]]),
    {_R3, _CT3} = comm_test:event(?MODULE, [1, Node, ST, [Wallet, 500]]),
    {_R4, _CT4} = comm_test:event(?MODULE, [1, Node, ST, [Wallet, 100]]),
    {_R5, _CT5} = comm_test:event(?MODULE, [1, Node, ST, [Wallet, 1000]]),
    {_R6, _CT6} = comm_test:event(?MODULE, [1, Node, ST, [Wallet, 400]]),
    {_R7, _CT7} = comm_test:event(?MODULE, [1, Node, ST, [Wallet, 600]]),
    {_R8, _CT8} = comm_test:event(?MODULE, [1, Node, ST, [Wallet, 800]]),
    {_R9, _CT9} = comm_test:event(?MODULE, [1, Node, ST, [Wallet, 700]]),
    {_R10, CT10} = comm_test:event(?MODULE, [1, Node, ST, [Wallet, 1400]]),
    CT10.

dc2_txns(Node, Wallet, ST) ->
    {_R1, _CT1} = comm_test:event(?MODULE, [1, Node, ST, [Wallet, 800]]),
    {_R2, _CT2} = comm_test:event(?MODULE, [1, Node, ST, [Wallet, 500]]),
    {_R3, _CT3} = comm_test:event(?MODULE, [1, Node, ST, [Wallet, 600]]),
    {_R4, _CT4} = comm_test:event(?MODULE, [2, Node, ST, [Wallet, 5000]]),
    {_R5, _CT5} = comm_test:event(?MODULE, [1, Node, ST, [Wallet, 900]]),
    {_R6, _CT6} = comm_test:event(?MODULE, [1, Node, ST, [Wallet, 1200]]),
    {_R7, _CT7} = comm_test:event(?MODULE, [1, Node, ST, [Wallet, 2000]]),
    {_R8, _CT8} = comm_test:event(?MODULE, [1, Node, ST, [Wallet, 800]]),
    {_R9, _CT9} = comm_test:event(?MODULE, [1, Node, ST, [Wallet, 900]]),
    {_R10, CT10} = comm_test:event(?MODULE, [1, Node, ST, [Wallet, 1400]]),
    CT10.


dc3_txns(Node, Wallet, ST) ->
    {_R1, _CT1} = comm_test:event(?MODULE, [1, Node, ST, [Wallet, 1800]]),
    {_R2, _CT2} = comm_test:event(?MODULE, [1, Node, ST, [Wallet, 1000]]),
    {_R3, _CT3} = comm_test:event(?MODULE, [1, Node, ST, [Wallet, 1100]]),
    {_R4, _CT4} = comm_test:event(?MODULE, [1, Node, ST, [Wallet, 900]]),
    {_R5, _CT5} = comm_test:event(?MODULE, [1, Node, ST, [Wallet, 1200]]),
    {_R6, _CT6} = comm_test:event(?MODULE, [1, Node, ST, [Wallet, 2000]]),
    {_R7, _CT7} = comm_test:event(?MODULE, [1, Node, ST, [Wallet, 1300]]),
    {_R8, _CT8} = comm_test:event(?MODULE, [1, Node, ST, [Wallet, 900]]),
    {_R9, CT9} = comm_test:event(?MODULE, [1, Node, ST, [Wallet, 1400]]),
    CT9.

%%%====================================
%%% Callbacks
%%%====================================
handle_event([1, Node, ST, AppArgs]) ->
    [Wallet, N] = AppArgs,
    {Res1, {_Tx1, CT1}} = wallet:debit(Node, Wallet, N, ST),
    {ok, [Res], _CT} = rpc:call(Node, antidote, read_objects, [ignore, [], [Wallet]]),

    ?DEBUG_LOG(io_lib:format("~n:::::::::::::::::: DEBIT EVENT (1) FOR ::::::::::::::::::::
            node: ~w ~n ~p ~n WITH RESULT: ~p [~p]", [Node, AppArgs, Res, Res1])),

    {Res1, CT1};

handle_event([2, Node, ST, AppArgs]) ->
    [Wallet, N] = AppArgs,
    {Res2, {_Tx2, CT2}} = wallet:credit(Node, Wallet, N, ST),
    {ok, [Res], _CT} = rpc:call(Node, antidote, read_objects, [ignore, [], [Wallet]]),
    ?DEBUG_LOG(io_lib:format("~n:::::::::::::::::: CREDIT EVENT (2) FOR ::::::::::::::::::::
            node: ~w ~p~nWITH RESULT:~p [~p]~n at time ~p", [Node, AppArgs, Res, Res2, CT2])),

    {Res2, CT2};

handle_event([3, Node, ST, AppArgs]) ->
    [Wallet1, Wallet2, N] = AppArgs,
    CT = wallet:transfer(Node, Wallet1, Wallet2, N, ST),
    CT.

handle_object_invariant(Node, [Wallet]) ->
    WalletVal =  wallet:get_val(Node, Wallet, ignore),
    ct:print("~nWallet value on ~p:~p~n", [Node, WalletVal]),
    ?assert(WalletVal >= 0),
    true.