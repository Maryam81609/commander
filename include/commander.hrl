-include("antidote.hrl").
-include("inter_dc_repl.hrl").

-define(DEBUG, false).
-define(DEBUG_LOG(Arg), case ?DEBUG of
                            true -> ct:print(Arg);
                            false -> skip
                        end).

-define(DEV(N), list_to_atom(lists:concat(["dev", N, "@127.0.0.1"]))).
%%-define(NODES_PATH, (comm_config:fetch(cluster_path))).
-define(COOKIE, antidote).
-define(COMMANDER, 'commander@127.0.0.1').
-define(ANTIDOTE, 'antidote@127.0.0.1').
%-define(DEBUG, true).

-type dc() :: [node()].

-record(upstream_event, {event_no :: pos_integer(),
                        event_data :: term(),
                        event_dc :: dcid(),
                        event_node :: node(),
                        %% TODO: merge following fields into one field: dict({txid(), {txn_commit_time :: non_neg_integer(), txn_snapshot_time :: dict()}})
                        event_commit_time :: non_neg_integer(),
                        event_snapshot_time :: dict:dict(),
                        event_txns :: [txid()]}).

-type upstream_event() :: #upstream_event{}.

-record(downstream_event, {event_dc :: dcid(),
                           event_node :: node(),
                           event_original_dc :: dcid(),
                           event_commit_time :: non_neg_integer(),
                           event_snapshot_time :: dict:dict(),
                           event_data :: [term()], %% => event_txns :: [inetrdc_txn()]; event_txns list contains partial transactions
                           event_txns :: [txid()]}). %% => event_txid :: txid()

%%-type downstream_event() :: #downstream_event{}.

-record(local_event, {
  event_no :: pos_integer(),
  event_dc :: dcid(),
  event_node :: node(),
  event_commit_time :: non_neg_integer(),
  event_snapshot_time :: dict:dict(),
  event_txns :: [txid()]}).

-type local_event() :: #local_event{}.

-record(remote_event, {
  event_dc :: dcid(),
  event_node :: node(),
  event_original_dc :: dcid(),
  event_commit_time :: non_neg_integer(),
  event_snapshot_time :: dict:dict(),
  event_txns :: [txid()]}).

-type remote_event() :: #remote_event{}.

-type event() :: local_event() | remote_event().

-record(execution, {id :: non_neg_integer(),
                    trace :: [event()]}).
-type execution() :: #execution{}.
-type delay_seq() :: [non_neg_integer()].
-type phase() :: record | replay | init_test.

-record(exec_state, {test_module :: atom(),
                     exec_events :: list(pos_integer())}).
-type exec_state() :: #exec_state{}.

-record(comm_state, {%%% Common fields
                    txn_ack_num :: dict:dict(), %% {TxId, DcId} -> Integer =< |updated_partitions|
                    txn_partial_num :: dict:dict(), %% TxId -> |updated_partitions|
                    server_client :: dict:dict(), %% server_node -> client_node
                    scheduler :: atom(),
                    txns_data :: dict:dict(), %%txId -> [{local, localData}, {remote,list(partialTxns)}]
                    initial_exec :: execution(),
                    curr_exec :: execution(), %% Shows the scheduled execution is getting replayed
                    phase :: phase(),
                    %%% Used by Replayer
                    recent_tx :: txid(),
                    %txn_map :: dict(), %% {key:OriginalTxId{snapshot_time, server_pid}, val:NewInterDCTxn}
                    clusters :: [list()], %% riak_test clusters
                    replay_history :: [execution()], %% Consider keeping only the list of event indices in the original exec
                    exec_counter :: non_neg_integer(), %% Name the recorded files
                    %%% Used by Recorder to record the initial execution
                    upstream_events :: [upstream_event()],
                    dep_clock_prgm :: dict:dict(), %% tx_id -> [{st, vectorclock}, {ct, vectorclock}]}
                    dep_txns_prgm :: dict:dict(), %% tx_id -> list(tx_id)
                    %%% Used by Scheduler
                    curr_delay_seq :: delay_seq(),
                    delay_direction :: atom(),
                    ct_config :: list()}).

-record(replay_state, {
  scheduler :: atom(),
  txns_data :: dict:dict(), %%txId -> [{local, localData}, {remote,list(partialTxns)}]
  txn_map :: dict:dict(),
  sch_count :: pos_integer(),
  clusters :: list(),
  dcs :: list(dc()),
  latest_txids :: [txid()]
}).

-record(delay_schlr_state, {
  cons_model :: atom(), %% cc | sr | ec (not supported now)
  event_count_total :: pos_integer(),
  curr_sch :: list(term()),
  dependency :: dict:dict(),
  orig_sch_sym_main :: list(term()), %% constant
  orig_sch_sym :: list(term()),
  orig_event_index :: non_neg_integer(),
  common_prefix_event_index :: non_neg_integer(),
  delayed_event_index :: non_neg_integer(),
  logical_ss :: dict:dict(),
  dcs :: list(),
  schedule_count::non_neg_integer(),
  bound :: pos_integer(),               %% bounds the total number of schedules
  common_prfx_bound :: pos_integer(),   %% bounds the number of schedules with a common prefix
  curr_delay_seq :: delay_seq(),        %% delay sequence for delaying original events
  curr_delayed_delay_seq :: delay_seq(),%% delay sequence for delaying delayed event
  delayed :: list(term()),
  delayed2 :: list(term()),
  delayed_main :: list(term()), %% This maintains the originally delayed event which is being delayed again recursively using delayed and delayed2
  delay_bound :: non_neg_integer(),
  delayed_count :: non_neg_integer(),
  common_prfx_schl :: list(term()),
  common_prfx_schl_cnt :: non_neg_integer(),
  delayer :: atom(), %% regular | delay
  dep_txns_prgm :: dict:dict(), %% tx_id -> list(tx_id)
  delay_sequencer :: atom(),

  %%% Used in rv16 branch
  curr_event_index :: non_neg_integer(),
  event_count :: non_neg_integer()
}).

-record(rand_schlr_state, {
    cons_model :: atom(),
  event_count_total :: pos_integer(),
  curr_sch :: list(term()),
  dependency :: dict:dict(),
  orig_sch_sym :: list(term()),
  remained :: list(),
  logical_ss :: dict:dict(),
  dcs :: list(),
  schedule_count::non_neg_integer(),
  bound :: pos_integer(),
  initial_seed :: term(),%% tx_id -> list(tx_id)
  dep_txns_prgm :: dict:dict()
}).

-record(verifier_state, {
  app_objects :: list(tuple()), %% {Key, Type, bucket}
  test_module :: atom()
}).