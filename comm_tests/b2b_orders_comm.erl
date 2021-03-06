-module(b2b_orders_comm).

-behavior(comm_test).

-include_lib("eunit/include/eunit.hrl").

%% API
-export([check/1,
    handle_event/1,
    handle_object_invariant/2]).

-define(INITBUDGET, 270000).
-define(ITEMS, [{shirt, antidote_crdt_counter, bucket}, {pants, antidote_crdt_counter, bucket}, {dress, antidote_crdt_counter, bucket}]).
-define(UNITCOST, [{shirt, 200}, {pants, 350}, {dress, 400}]).
-define(STOREIDS, [store1]).
-define(BUDGET_UPDATE1, 20000).
-define(BUDGET_UPDATE3, 30000).

check(Config) ->
    ct:print("Entered check!"),
    [Node1, Node2, Node3] = proplists:get_value(sut_nodes, Config),

    main_test([Node1, Node2, Node3]),

    pass.

main_test([Node1, Node2, Node3]) ->
    %%% init budget for every store on every DC
    {ok, CTInit} = comm_test:event(?MODULE, [1, Node1, ignore, [?INITBUDGET]]),

    CT1 = dc1_txns(Node1, [], [CTInit]),
    CT2 = dc2_txns(Node2, [], [CTInit]),
    CT3 = dc3_txns(Node3, [], [CTInit]),

    %%% Specify invariant objects
    BudgetObjs = rpc:call(Node1, b2b_orders, create_budget_obj, [all]),
    comm_test:objects(?MODULE, BudgetObjs),

    Time =
    dict:merge(fun(_K, T1, T2) ->
                 max(T1, T2)
               end, CT1,
               dict:merge(fun(_K, T1, T2) ->
                            max(T1, T2)
                          end, CT2, CT3)),

    [BudgetObj] = BudgetObjs,
    Vals = [rpc:call(Node, b2b_orders, get_val, [BudgetObj, Time]) || Node <- [Node1, Node2, Node3]],

    Quiescence_val = lists:usort(Vals),
    ?assertMatch(Quiescence_val, [hd(Vals)]),
    pass.

dc1_txns(Node, _Orders, [STInit]) ->
    Order1 = {dress, 50, "Jan. 29"},
    Order2 = {pants, 50, "Feb. 10"},
    Order3 = {shirt, 50, "Feb. 15"},
    Order4 = {shirt, 150, "March. 15"},
    Order5 = {pants, 50, "Aug. 23"},

    {_Res1, CT1} = comm_test:event(?MODULE, [3, Node, STInit, [{store1, [Order1]}]]),
    {_Res2, CT2} = comm_test:event(?MODULE, [3, Node, STInit, [{store1, [Order2]}]]),
    {_Res3, CT3} = comm_test:event(?MODULE, [3, Node, STInit, [{store1, [Order3]}]]),
    {_Res4, CT4} = comm_test:event(?MODULE, [3, Node, STInit, [{store1, [Order4]}]]),
    {_Res5, CT5} = comm_test:event(?MODULE, [3, Node, STInit, [{store1, [Order5]}]]),

    vc_max(vc_max(CT1, CT2), vc_max(vc_max(CT4, CT3), CT5)).

dc2_txns(Node, _Orders, [STInit]) ->
    Order1 = {shirt, 80, "Feb. 19"},
    Order2 = {pants, 50, "Feb. 28"},
    Order3 = {shirt, 150, "April. 19"},
    Order4 = {dress, 60, "March. 29"},
    Order5 = {pants, 50, "Jan. 28"},

    {_Res1, CT1} = comm_test:event(?MODULE, [3, Node, STInit, [{store1, [Order1]}]]),
    {_Res2, CT2} = comm_test:event(?MODULE, [3, Node, STInit, [{store1, [Order2]}]]),
    {_Res3, CT3} = comm_test:event(?MODULE, [3, Node, STInit, [{store1, [Order3]}]]),
    {_Res4, CT4} = comm_test:event(?MODULE, [3, Node, STInit, [{store1, [Order4]}]]),
    {_Res2, CT5} = comm_test:event(?MODULE, [3, Node, STInit, [{store1, [Order5]}]]),

    vc_max(vc_max(CT1, CT2), vc_max(vc_max(CT4, CT3), CT5)).

dc3_txns(Node, _Orders, [STInit]) ->
    Order1 = {dress, 50, "March. 17"},
    Order2 = {pants, 30, "Feb. 01"},
    Order3 = {shirt, 50, "April. 19"},
    Order4 = {shirt, 75, "June. 21"},
    Order5 = {pants, 40, "Sept. 11"},
    Order6 = {pants, 30, "Feb. 01"},

    {_Res, _CT} = comm_test:event(?MODULE, [1, Node, STInit, [30000]]),
    {_Res1, CT1} = comm_test:event(?MODULE, [3, Node, STInit, [{store1, [Order1]}]]),
    {_Res2, CT2} = comm_test:event(?MODULE, [3, Node, STInit, [{store1, [Order2]}]]),
    {_Res3, CT3} = comm_test:event(?MODULE, [3, Node, STInit, [{store1, [Order3]}]]),
    {_Res4, CT4} = comm_test:event(?MODULE, [3, Node, STInit, [{store1, [Order4]}]]),
    {_Res5, CT5} = comm_test:event(?MODULE, [3, Node, STInit, [{store1, [Order5]}]]),
    {_Res6, CT6} = comm_test:event(?MODULE, [3, Node, STInit, [{store1, [Order6]}]]),

    vc_max(vc_max(vc_max(CT1, CT2), CT3), vc_max(vc_max(CT4, CT5), CT6)).

%%%=======================================
%%% Commander Callback functions
%%%=======================================
%%% init store budgets
handle_event([1, Node, ST, AppArgs]) ->
    [Val] = AppArgs,
    {ok, CT1} = rpc:call(Node, b2b_orders, init_budget, [ST, Val]),
    {ok, CT1};

%%% init stock availability
handle_event([2, Node, ST, AppArgs]) ->
    [Count] = AppArgs,
    {ok, CT1} = rpc:call(Node, b2b_orders, init_availability, [ST, Count]),
    {ok, CT1};

handle_event([3, Node, ST, AppArgs]) ->
    [{StoreId, Orders}] = AppArgs,
    {_Res, {_TxId, CT1}} = rpc:call(Node, b2b_orders, place_order, [ST, {StoreId, Orders}]),
    {ok, CT1}.


%%%=======================================
%%% Invariant specification
%%%=======================================
handle_object_invariant(Node, [BudgetObj]) ->
    {Key, _Type, _} = BudgetObj,
    Budget =  rpc:call(Node, b2b_orders, get_val, [BudgetObj, ignore]),
    ct:print("~nBudget for Store: ~w on node: ~w became: ~p~n", [Key, Node, Budget]),
    ?assert(Budget >= 0),
    true.

%%%=======================================
%%% Internal functions
%%%=======================================
vc_max(VC1, VC2) ->
    dict:merge(fun(_K, V1, V2) ->
                    if
                        V1 =< V2 -> V2;
                        true -> V1
                    end
               end, VC1, VC2).
