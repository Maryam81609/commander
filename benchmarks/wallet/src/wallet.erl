-module(wallet).

%% API
-export([credit/3, debit/3, transfer/4, get_val/2]).

-include_lib("eunit/include/eunit.hrl").

-define(DB_NODE, antidote_node).
-define(APP, wallet).

debit(Wallet, Amount, ST) ->
    {ok, AntNode} = application:get_env(?APP, ?DB_NODE),
    {ok, Tx1} = rpc:call(AntNode, antidote, start_transaction, [ST, []]),
    ok = rpc:call(AntNode, antidote, update_objects, [[{Wallet, decrement, Amount}], Tx1]),
    {ok, [Res1]} = rpc:call(AntNode, antidote, read_objects, [[Wallet], Tx1]),
    {ok, CT1} = rpc:call(AntNode, antidote, commit_transaction, [Tx1]),
    {Res1, {Tx1, CT1}}.

credit(Wallet, Amount, ST) ->
    {ok, AntNode} = application:get_env(?APP, ?DB_NODE),
    {ok, Tx2} = rpc:call(AntNode, antidote, start_transaction, [ST, []]),
    ok = rpc:call(AntNode, antidote, update_objects, [[{Wallet, increment, Amount}], Tx2]),
    {ok, [Res2]} = rpc:call(AntNode, antidote, read_objects, [[Wallet], Tx2]),
    {ok, CT2} = rpc:call(AntNode, antidote, commit_transaction, [Tx2]),
    {Res2, {Tx2, CT2}}.

transfer(FromWallet, ToWallet, Amount, ST) ->
    {ok, AntNode} = application:get_env(?APP, ?DB_NODE),
    {ok, TxTrnsfr} = rpc:call(AntNode, antidote, start_transaction, [ST, []]),
    ok = rpc:call(AntNode, antidote, update_objects, [[{FromWallet, decrement, Amount}, {ToWallet, increment, Amount}], TxTrnsfr]),
    {ok, CT} = rpc:call(AntNode, antidote, commit_transaction, [TxTrnsfr]),
    CT.

get_val(Wallet, Clock) ->
    {ok, AntNode} = application:get_env(?APP, ?DB_NODE),
    {ok, Tx} = rpc:call(AntNode, antidote, start_transaction, [Clock, []]),
    {ok, [Res]} = rpc:call(AntNode, antidote, read_objects, [[Wallet], Tx]),
    {ok, _CT1} = rpc:call(AntNode, antidote, commit_transaction, [Tx]),
    Res.