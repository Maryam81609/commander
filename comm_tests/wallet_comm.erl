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

check(Config) ->
    ct:print("Entered check!"),
    [Node1, Node2, Node3] = proplists:get_value(sut_nodes, Config),

    main_test([Node1, Node2, Node3]),

    pass.

main_test([Node1, Node2, Node3]) ->
    Key = wallet_key,
    Wallet = {Key, antidote_crdt_counter, bucket},

    %%% Specify invariant objects
    comm_test:objects(?MODULE, [Wallet]),

    {_Re, CT} = comm_test:event(?MODULE, [2, Node1, ignore, [Wallet, 26500]]),

    [?assertEqual(rpc:call(Node, wallet, get_val, [Node, Wallet, CT]), 26500) || Node <- [Node1, Node2, Node3]],

    CT1 = dc1_txns(Node1, Wallet, CT),
    CT2 = dc2_txns(Node2, Wallet, CT),
    CT3 = dc3_txns(Node3, Wallet, CT),

    Time = dict:merge(fun(_K, T1, T2) -> max(T1, T2) end,
        CT1,
        dict:merge(fun(_K, T1, T2) -> max(T1, T2) end, CT2, CT3)),

    Vals = [rpc:call(Node, wallet, get_val, [Node, Wallet, Time]) || Node <- [Node1, Node2, Node3]],
    ct:print("Vals: ~w", [Vals]),

    Quiescence_val = lists:usort(Vals),
    ?assertMatch(Quiescence_val, [hd(Vals)]),

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
    {Res1, {_Tx1, CT1}} = rpc:call(Node, wallet, debit, [Node, Wallet, N, ST]),
    {Res1, CT1};

handle_event([2, Node, ST, AppArgs]) ->
    [Wallet, N] = AppArgs,
    {Res2, {_Tx2, CT2}} = rpc:call(Node, wallet, credit, [Node, Wallet, N, ST]),
    {Res2, CT2};

handle_event([3, Node, ST, AppArgs]) ->
    [Wallet1, Wallet2, N] = AppArgs,
    CT = rpc:call(Node, wallet, transfer, [Node, Wallet1, Wallet2, N, ST]),
    CT.

handle_object_invariant(Node, [Wallet]) ->
    WalletVal =  rpc:call(Node, wallet, get_val, [Node, Wallet, ignore]),
    ct:print("~nWallet value on ~p:~p~n", [Node, WalletVal]),
    ?assert(WalletVal >= 0),
    true.