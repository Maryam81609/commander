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
    Clusters = proplists:get_value(clusters, Config),
    [Node1, Node2, Node3 | _Nodes] =  [ hd(Cluster)|| Cluster <- Clusters ],

    main_test(Node1, Node2, Node3),

    %% function check must return pass
    pass.

handle_event([_EventNo, _Node, _CausalClock, _AppArgs]) ->
    %% Call the application under test here
    _Result.

handle_object_invariant(_Node, _InvariantArgs) ->
    %% Write any Erlang code and EUnite assertions here to check the invariant on the given node
    %% must return true
    true.

%%% =============================
%%% Internal functions
%%% =============================
main_test(Node1, Node2, Node3) ->

    %% InvariantArgs is a list of
    comm_test:objects(?MODULE, _InvariantArgs),

    %% Add dci_txns(_Args) if you have more than three DCs
    dc1_txns(Node1, _Args),%%Node1, [], [CTInit]),
    dc2_txns(Node2, _Args),%%Node2, [], [CTInit]),
    dc3_txns(Node3, _Args).%%Node3, [], [CTInit]),

dc1_txns(Node, _Args) ->
    %%% Wrap a transaction in an event
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