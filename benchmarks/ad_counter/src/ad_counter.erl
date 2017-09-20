%%%-------------------------------------------------------------------
%%% @author maryam
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 27. May 2016 3:29 PM
%%%-------------------------------------------------------------------
-module(ad_counter).
-author("maryam").

%% API
-export([view_ad/2, get_val/2]).

-include_lib("eunit/include/eunit.hrl").

-define(DB_NODE, antidote_node).
-define(APP, ad_counter).

view_ad(Ad, Clock) ->
  {ok, AntNode} = application:get_env(?APP, ?DB_NODE),
  {ok, TxId} = rpc:call(AntNode, antidote, start_transaction, [Clock, []]),
  {ok, [Res1]} = rpc:call(AntNode, antidote, read_objects, [[Ad], TxId]),
  if
    Res1<5 ->
      ok = rpc:call(AntNode, antidote, update_objects, [[{Ad, increment, 1}], TxId]);
    true ->
      ok = rpc:call(AntNode, antidote, update_objects, [[{Ad, increment, 1}, {Ad, decrement, 1}], TxId])
  end,
  {ok, [Res]} = rpc:call(AntNode, antidote, read_objects, [[Ad], TxId]),
  {ok, CT} = rpc:call(AntNode, antidote, commit_transaction, [TxId]),
  {Res, {TxId, CT}}.

get_val(Ad, Clock) ->
  {ok, AntNode} = application:get_env(?APP, ?DB_NODE),
  {ok, Tx} = rpc:call(AntNode, antidote, start_transaction, [Clock, []]),
  {ok, [Res]} = rpc:call(AntNode, antidote, read_objects, [[Ad], Tx]),
  {ok, _CT1} = rpc:call(AntNode, antidote, commit_transaction, [Tx]),
  Res.