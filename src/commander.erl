%%%-------------------------------------------------------------------
%%% @author maryam
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 09. May 2017 5:44 PM
%%%-------------------------------------------------------------------
-module(commander).
-author("maryam").

%% API
-export([lager_test/0,
  node_start/0]).

-include_lib("common_test/include/ct.hrl").

lager_test() ->
  lager:info("~nlager is working!").

node_start() ->
  {ok, Node} = ct_slave:start('dev1@127.0.0.1',
                              [{monitor_master, true},
                                {erl_flags, "-smp"}]),
  io:format("~nNode: ~p", [Node]).

