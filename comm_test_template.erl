-module(comm_test_template).

-behavior(comm_test).

-include_lib("eunit/include/eunit.hrl").

%% comm_test callbacks
-export([check/1,
    handle_event/1,
    handle_object_invariant/2]).

%%% =============================
%%% comm_test callback functions
%%% =============================
check(Config) ->
    [Node1, Node2, Node3] = proplists:get_value(sut_nodes, Config),

    main_test([Node1, Node2, Node3]),

    %% function check must return pass
    pass.

handle_event([_EventNo, _Node, _CausalClock, _AppArgs]) ->
    %% Call the application under test here
    _Result.

handle_object_invariant(_Node, _InvariantArgs) ->
    %% Write any Erlang code and EUnite assertions here to check the invariant on the specified node
    %% must return true
    true.

%%% =============================
%%% Internal functions
%%% =============================
main_test([Node1, Node2, Node3]) ->

    %% InvariantArgs is a list of arguments required for handle_object_invariant
    comm_test:objects(?MODULE, _InvariantArgs),

    %% Add dci_txns(_Args) if you have more than three DCs
    dc1_txns(Node1, _Args),
    dc2_txns(Node2, _Args),
    dc3_txns(Node3, _Args).

dc1_txns(Node, _Args) ->
    %%% Wrap an application API call in an event
    %%%%% EventNo :: pos_integer(), differentiate events by EventNo
    %%%%% Node specifies the node a transaction is executing on
    %%%%% CausalClock :: vectorclock() | ignore, specifies the causal dependency of the transaction
    %%%%% AppArgs :: list(), arguments that are passed to the application under test
    %%% First three arguments are mandatory
    comm_test:event(?MODULE, [_EventNo, Node, _CausalClock, _AppArgs]),
    _Result.

dc2_txns(_Node, _Args) ->
    _Result.

dc3_txns(_Node, _Args) ->
    _Result.