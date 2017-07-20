%%%-------------------------------------------------------------------
%%% @author maryam
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 27. May 2016 4:16 PM
%%%-------------------------------------------------------------------
-module(ad_counter_comm).

-behavior(comm_test).

-include_lib("eunit/include/eunit.hrl").
%% API
-export([check/1, handle_event/1, handle_object_invariant/2]).

check(Config) ->
  Clusters = proplists:get_value(clusters, Config),
  [Node1, Node2, Node3 | _Nodes] =  [ hd(Cluster)|| Cluster <- Clusters ],
  main_test(Node1, Node2, Node3).

main_test(Node1, Node2, Node3) ->
%%  MaxView = 5,
  Key = ad_key,
  Ad = {Key, antidote_crdt_counter, bucket},
  Pid = self(),

  %%% Specify invariant objects
  comm_test:objects(?MODULE, [Ad]),

  CT1 = dc1_txns(Node1, Ad, Pid, ignore),
  CT2 = dc2_txns(Node2, Ad, Pid, ignore),
  CT3 = dc3_txns(Node3, Ad, Pid, vc_max(CT1,CT2)),

  Time = dict:merge(fun(_K, T1, T2) -> max(T1, T2) end,
                      CT1,
                      dict:merge(fun(_K, T1, T2) -> max(T1, T2) end, CT2, CT3)),

  Vals = [ad_counter:get_val(Node, Ad, Time) || Node <- [Node1, Node2, Node3]],

  Quiescence_val = lists:usort(Vals),
  ?assertMatch(Quiescence_val, [hd(Vals)]),
  pass.

dc1_txns(Node, Ad, _ReplyTo, ST) ->
  {_Res1, CT1} = comm_test:event(?MODULE, [1, Node, ST, [Ad]]),
  {Res2, _CT2} = comm_test:event(?MODULE, [1, Node, ST, [Ad]]),
  CT1.

dc2_txns(Node, Ad, _ReplyTo, ST) ->
  {_Res1, CT1} = comm_test:event(?MODULE, [1, Node, ST, [Ad]]),
  {Res2, CT2} = comm_test:event(?MODULE, [1, Node, ST, [Ad]]),
  vc_max(CT1, CT2).

dc3_txns(Node, Ad, _ReplyTo, ST) ->
  {_Res1, _CT1} = comm_test:event(?MODULE, [1, Node, ST, [Ad]]),
  {Res2, CT2} = comm_test:event(?MODULE, [1, Node, ignore, [Ad]]),
  CT2.

%%%====================================
%%% Callbacks
%%%====================================
handle_event([1, Node, ST, AppArgs]) ->
  [Ad] = AppArgs,
  {Res1, {_Tx1, CT1}} = ad_counter:view_ad(Node, Ad, ST),
  {Res1, CT1}.

handle_object_invariant(Node, [Ad]) ->
  AdVal = ad_counter:get_val(Node, Ad, ignore),
  %%% if assert fails inform commander to provide a counter example
  ct:print("~nAd value on ~p:~p~n", [Node, AdVal]),
  ?assert(AdVal =< 5 ),
  true.
%%%=================================

vc_max(VC1, VC2) ->
    dict:merge(fun(_K, V1, V2) ->
                    if
                      V1 =< V2 -> V2;
                      true -> V1
                    end
               end, VC1, VC2).