%%%-------------------------------------------------------------------
%%% @author maryam
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. May 2017 11:37 AM
%%%-------------------------------------------------------------------
-module(wallet_no_comm).
-author("maryam").

-behavior(comm_test).

-include("../include/commander.hrl").
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

    main_test(Node1, Node2, Node3),
    pass.

main_test(Node1, Node2, Node3) ->
    Key = wallet_key,
    Wallet = {Key, antidote_crdt_counter, bucket},
%%    Wallet = {Key, riak_dt_pncounter, bucket},

    %%% Specify invariant objects
    %%comm_test:objects(?MODULE, [Wallet]),

    if
        ?DEBUG -> ct:print("Node1: ~p", [Node1]);
        true -> skip
    end,

    {_Re, CT} = handle_event([2, Node1, ignore, [Wallet, 26500]]),

    if
        ?DEBUG ->
            ct:print("~n^^^^^^^^^^CT: ~w^^^^^^^^^^^~n", [dict:to_list(CT)]);
        true -> skip
    end,

    [?assertEqual(get_val(Node, Wallet, CT), 26500) || Node <- [Node1, Node2, Node3]],

    CT1 = dc1_txns(Node1, Wallet, CT),
    CT2 = dc2_txns(Node2, Wallet, CT),
    CT3 = dc3_txns(Node3, Wallet, CT),

    Time = dict:merge(fun(_K, T1, T2) -> max(T1, T2) end,
        CT1,
        dict:merge(fun(_K, T1, T2) -> max(T1, T2) end, CT2, CT3)),

    Vals = [get_val(Node, Wallet, Time) || Node <- [Node1, Node2, Node3]],
    ct:print("Vals: ~w", [Vals]),
    Quiescence_val = lists:usort(Vals),
    ?assertMatch(Quiescence_val, [23700]),
    ct:print("Quiesence val: ~p", [Quiescence_val]),

    if
        ?DEBUG ->
            ct:print("CT1:~w~nCT2:~w~nCT3:~w~n~nTime:~w~n",
                [dict:to_list(CT1), dict:to_list(CT2), dict:to_list(CT3), dict:to_list(Time)]),
            ct:print("Val on node 1: ~p", [get_val(Node1, Wallet, Time)]),
            %%  Quiescence_val = lists:usort(Vals),
            %%  ?assertMatch(Quiescence_val, [hd(Vals)]),
            ct:print("Cookie: ~p, Node: ~p", [erlang:get_cookie(), node()]),
            ct:print("Self: ~p~n riak_test: ~p", [self(), whereis(riak_test)]);
        true -> skip
    end,

    pass.

get_val(Node, Wallet, Clock) ->
    {ok, Tx} = rpc:call(Node, antidote, start_transaction, [Clock, []]),
    {ok, [Res]} = rpc:call(Node, antidote, read_objects, [[Wallet], Tx]),
    {ok, _CT1} = rpc:call(Node, antidote, commit_transaction, [Tx]),
    Res.

dc1_txns(Node, Wallet, ST) ->
    {_, CT} = handle_event([1, Node, ST, [Wallet, 200]]),
    CT.
%%    {_R1, _CT1} = comm_test:event(?MODULE, [1, Node, ST, [Wallet, 200]]),
%%    {_R2, _CT2} = comm_test:event(?MODULE, [1, Node, ST, [Wallet, 300]]),
%%    {_R3, _CT3} = comm_test:event(?MODULE, [1, Node, ST, [Wallet, 500]]),
%%    {_R4, _CT4} = comm_test:event(?MODULE, [1, Node, ST, [Wallet, 100]]),
%%    {_R5, _CT5} = comm_test:event(?MODULE, [1, Node, ST, [Wallet, 1000]]),
%%    {_R6, _CT6} = comm_test:event(?MODULE, [1, Node, ST, [Wallet, 400]]),
%%    {_R7, _CT7} = comm_test:event(?MODULE, [1, Node, ST, [Wallet, 600]]),
%%    {_R8, _CT8} = comm_test:event(?MODULE, [1, Node, ST, [Wallet, 800]]),
%%    {_R9, _CT9} = comm_test:event(?MODULE, [1, Node, ST, [Wallet, 700]]),
%%    {_R10, CT10} = comm_test:event(?MODULE, [1, Node, ST, [Wallet, 1400]]),
%%    CT10.

dc2_txns(Node, Wallet, ST) ->
    {_, CT} = handle_event([1, Node, ST, [Wallet, 800]]),
    CT.
%%    {_R1, _CT1} = comm_test:event(?MODULE, [1, Node, ST, [Wallet, 800]]),
%%    {_R2, _CT2} = comm_test:event(?MODULE, [1, Node, ST, [Wallet, 500]]),
%%    {_R3, _CT3} = comm_test:event(?MODULE, [1, Node, ST, [Wallet, 600]]),
%%    {_R4, _CT4} = comm_test:event(?MODULE, [2, Node, ST, [Wallet, 5000]]),
%%    {_R5, _CT5} = comm_test:event(?MODULE, [1, Node, ST, [Wallet, 900]]),
%%    {_R6, _CT6} = comm_test:event(?MODULE, [1, Node, ST, [Wallet, 1200]]),
%%    {_R7, _CT7} = comm_test:event(?MODULE, [1, Node, ST, [Wallet, 2000]]),
%%    {_R8, _CT8} = comm_test:event(?MODULE, [1, Node, ST, [Wallet, 800]]),
%%    {_R9, _CT9} = comm_test:event(?MODULE, [1, Node, ST, [Wallet, 900]]),
%%    {_R10, CT10} = comm_test:event(?MODULE, [1, Node, ST, [Wallet, 1400]]),
%%    CT10.


dc3_txns(Node, Wallet, ST) ->
    {_, CT} = handle_event([1, Node, ST, [Wallet, 1800]]),
    CT.
%%    {_R1, _CT1} = comm_test:event(?MODULE, [1, Node, ST, [Wallet, 1800]]),
%%    {_R2, _CT2} = comm_test:event(?MODULE, [1, Node, ST, [Wallet, 1000]]),
%%    {_R3, _CT3} = comm_test:event(?MODULE, [1, Node, ST, [Wallet, 1100]]),
%%    {_R4, _CT4} = comm_test:event(?MODULE, [1, Node, ST, [Wallet, 900]]),
%%    {_R5, _CT5} = comm_test:event(?MODULE, [1, Node, ST, [Wallet, 1200]]),
%%    {_R6, _CT6} = comm_test:event(?MODULE, [1, Node, ST, [Wallet, 2000]]),
%%    {_R7, _CT7} = comm_test:event(?MODULE, [1, Node, ST, [Wallet, 1300]]),
%%    {_R8, _CT8} = comm_test:event(?MODULE, [1, Node, ST, [Wallet, 900]]),
%%    {_R9, CT9} = comm_test:event(?MODULE, [1, Node, ST, [Wallet, 1400]]),
%%    CT9.


%%%====================================
%%% Callbacks
%%%====================================
handle_event([1, Node, ST, AppArgs]) ->
    [Wallet, N] = AppArgs,
    {Res1, {_Tx1, CT1}} = wallet:debit(Node, Wallet, N, ST),
%%    {Key, Type, bucket} = Wallet,
    {ok, [Res], _CT} = rpc:call(Node, antidote, read_objects, [ignore, [], [Wallet]]),

    if
        ?DEBUG ->
            ct:print("~n:::::::::::::::::: DEBIT EVENT (1) FOR ::::::::::::::::::::
            node: ~w ~n ~p ~n WITH RESULT: ~p [~p]", [Node, AppArgs, Res, Res1]);
        true -> skip
    end,

%%    io:format("~n:::::::::::::::::: DEBIT EVENT (1) FOR ::::::::::::::::::::
%% node: ~w ~n ~p ~n WITH RESULT: ~p [~p]", [Node, AppArgs, Res, Res1]),

    {Res1, CT1};

handle_event([2, Node, ST, AppArgs]) ->
    [Wallet, N] = AppArgs,
    {Res2, {_Tx2, CT2}} = wallet:credit(Node, Wallet, N, ST),
%%    {_Key, _Type, bucket} = Wallet,
    {ok, [Res], _CT} = rpc:call(Node, antidote, read_objects, [ignore, [], [Wallet]]),

    if
        ?DEBUG ->
            ct:print("~n:::::::::::::::::: CREDIT EVENT (2) FOR ::::::::::::::::::::
            node: ~w ~p~nWITH RESULT:~p [~p]", [Node, AppArgs, Res, Res2]);
        true -> skip
    end,

    {Res2, CT2};

handle_event([3, Node, ST, AppArgs]) ->
    [Wallet1, Wallet2, N] = AppArgs,
    CT = wallet:transfer(Node, Wallet1, Wallet2, N, ST),
    CT.

handle_object_invariant(Node, [Wallet]) ->
    WalletVal =  get_val(Node, Wallet, ignore),
    ct:print("~nWallet value:~p~n", [WalletVal]),
%%    io:format("~nWallet value:~p~n", [WalletVal]),
    ?assert(WalletVal >= 0),
    true.