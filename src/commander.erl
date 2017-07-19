-module(commander).

-behaviour(gen_server).

-include("../include/commander.hrl").
-include_lib("eunit/include/eunit.hrl").

%% Public API
-export([start_link/2,
        stop/0,
        update_upstream_event_data/1,
        get_upstream_event_data/1,
        get_downstream_event_data/1,
        update_transactions_data/2,
        get_scheduling_data/0,
        phase/0,
        get_clusters/1,
        check/2,
        run_next_test1/0,
        test_initialized/0,
        update_replay_txns_data/3,
        acknowledge_delivery/3,
        test_passed/0,
        passed_test_count/0,
        get_app_objects/2,
        display_result/0,
        write_time/2,
        set_ct_config/1,
        get_ct_config/0,
        set_server_client/1,
        set_txn_partial_num/1,
        reset_txn_ack_num/2,
        get_txns_data/0]).

%% gen_server callbacks
-export([init/1,
        handle_cast/2,
        handle_call/3,
        handle_info/2,
        code_change/3,
        terminate/2
		]).

-define(SERVER, ?MODULE).

%%%====================================
%%% Public API
%%%====================================
start_link(Scheduler, DelayDirection) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Scheduler, DelayDirection], []).

get_txns_data() ->
    gen_server:call(?SERVER, {get_txns_data}).

reset_txn_ack_num({TxId, EventDc}, Erase) ->
    gen_server:call(?SERVER, {reset_txn_ack_num, {{TxId, EventDc}, Erase}}).

set_txn_partial_num({TxId, PartialNum}) ->
    gen_server:call(?SERVER, {set_txn_partial_num, {TxId, PartialNum}}).

get_clusters(Clusters) ->
    gen_server:call(?SERVER, {get_clusters, Clusters}).

get_downstream_event_data(Data) ->
    gen_server:call(?SERVER, {get_downstream_event_data, {Data}}).

get_upstream_event_data(Data) ->
    gen_server:call(?SERVER, {get_upstream_event_data, {Data}}).

update_upstream_event_data(Data) ->
    gen_server:call(?SERVER, {update_upstream_event_data, {Data}}).

update_transactions_data(TxId, InterDcTxn) ->
    gen_server:call(?SERVER, {update_transactions_data, {TxId, InterDcTxn}}).

get_scheduling_data() ->
    gen_server:call(?SERVER, get_scheduling_data).

phase() ->
    gen_server:call(?SERVER, phase).

test_passed() ->
  gen_server:call(?SERVER, test_passed).

passed_test_count() ->
  gen_server:call(?SERVER, passed_test_count).

get_app_objects(Mod, Objs) ->
  gen_server:call(?SERVER, {get_app_objects, {Mod, Objs}}).

set_server_client(Cli_Clusters) ->
    gen_server:call(?SERVER, {set_server_client, {Cli_Clusters}}).

check(SchParam, Bound) ->
  gen_server:cast(?SERVER, {check, {SchParam, Bound}}).

run_next_test1() ->
  gen_server:cast(?SERVER, run_next_test1).

test_initialized() ->
  gen_server:cast(?SERVER, test_initialized).

update_replay_txns_data(LocalTxnData, InterDCTxn, TxId) ->
  gen_server:cast(?SERVER, {update_replay_txns_data, {LocalTxnData, InterDCTxn, TxId}}).

acknowledge_delivery(TxId, RecDC, Timestamp) ->
  gen_server:cast(?SERVER, {acknowledge_delivery, {TxId, RecDC, Timestamp}}).

display_result() ->
  gen_server:call(?SERVER, display_result).

set_ct_config(Config) ->
    gen_server:call(?SERVER, {set_ct_config, {Config}}).

get_ct_config() ->
    gen_server:call(?SERVER, get_ct_config).

stop() ->
    gen_server:cast(?SERVER, stop).

%%%====================================
%%% Callbacks
%%%====================================
init([Scheduler, DelayDirection]) ->
    ct:print("Commander started on: ~p", [node()]),
    ExecId = 1,
    NewState = comm_recorder:init_record(ExecId,
        #comm_state{scheduler = Scheduler,
            delay_direction = DelayDirection,
            txn_partial_num = dict:new(),
            txn_ack_num = dict:new()}),
    {ok, NewState}.

handle_call({get_txns_data}, _From, State) ->
    {reply, State#comm_state.txns_data, State};

handle_call({reset_txn_ack_num, {{TxId, EventDc}, _Erase}}, _From, State) ->
    TxnAckNum = State#comm_state.txn_ack_num,
    NewTxnAckNum = dict:erase({TxId, EventDc}, TxnAckNum),
    NewState = State#comm_state{txn_ack_num = NewTxnAckNum},
    {reply, ok, NewState};

handle_call({set_txn_partial_num, {TxId, PartialNum}}, _From, State) ->
    TxnPartNum = State#comm_state.txn_partial_num,
    NewTxnPartNum = dict:store(TxId, PartialNum, TxnPartNum),
    NewState = State#comm_state{txn_partial_num = NewTxnPartNum},
    ?DEBUG_LOG(io_lib:format("Set txn_partial_num; ~p", [dict:to_list(NewState#comm_state.txn_partial_num)])),
    {reply, ok, NewState};

handle_call({set_server_client, {Cli_Clusters}}, _From, State) ->
    Serv_Cli = lists:foldl(fun({ClientNode, ServerNodes}, Res) ->
                           lists:foldl(fun(ServerNode, Res2) ->
                                        dict:store(ServerNode, ClientNode, Res2)
                                       end, Res, ServerNodes)
                           end, dict:new(), Cli_Clusters),

    NewState = State#comm_state{server_client = Serv_Cli},
    {reply, ok, NewState};

handle_call(display_result, _From, State) ->
  Scheduler = State#comm_state.scheduler,
  display_check_result(Scheduler),
  {reply, ok, State};

handle_call({get_app_objects, {Mod, Objs}}, _From, State) ->
  {ok, _} = comm_verifier:start_link(Mod, Objs),
  {reply, ok, State};

handle_call(passed_test_count, _From, State) ->
  Scheduler = State#comm_state.scheduler,
  PassedTestCount = Scheduler:schedule_count(),
  {reply, PassedTestCount, State};

handle_call(test_passed, _From, State) ->
  {reply, ok, State#comm_state{phase=init_test}};

handle_call(get_scheduling_data, _From, State) ->
  {execution, 1, OrigSch} = State#comm_state.initial_exec,
  OrigSymSch = comm_utilities:get_symbolic_sch(OrigSch),
  {reply, OrigSymSch, State};

handle_call({get_clusters, Clusters}, _From, State) ->
    %%Only can get here in the initial run while setting the environment up
    NewState = State#comm_state{clusters = Clusters},
    {reply, ok, NewState};

handle_call(phase, _From, State) ->
    Phase = State#comm_state.phase,
    {reply, Phase, State};

%% TODO: handle the case, where the transaction has no update operation
handle_call({get_downstream_event_data, {Data}}, _From, State) ->
%%    ?DEBUG_LOG(io_lib:format("Got downstream event data: ~p.", [Data])),
    {EventDc, EventNode, EventTxn} = Data,
    LogRecords = EventTxn#interdc_txn.log_records,
    LastLogRec = lists:last(LogRecords),
    LogOp = LastLogRec#log_record.log_operation,
    TxnId = LogOp#log_operation.tx_id,

    TxnPartNum = State#comm_state.txn_partial_num,
    TxnAckNum = State#comm_state.txn_ack_num,
    PartNum = dict:fetch(TxnId, TxnPartNum),

%%    ?DEBUG_LOG(io_lib:format("get_downstream_event_data::TxnAckNum: ~p", [TxnAckNum])),

    Res = dict:find({TxnId, EventDc}, TxnAckNum),
%%    ?DEBUG_LOG(io_lib:format("get_downstream_event_data-Res: ~p~n~p", [Res, {TxnId, EventDc}])),
    Res2 =
        case Res of
            error ->
                {ok, 0};
            _ -> Res
        end,
%%    ?DEBUG_LOG(io_lib:format("get_downstream_event_data-Res2: ~p~n~p", [Res2, {TxnId, EventDc}])),
    {NewAckNum, NewState1} =
        case Res2 of
            {ok, AckNum} when AckNum+1 == PartNum ->
                EventTxns = [TxnId],
                EventOriginalDc = EventTxn#interdc_txn.dcid,
                EventCommitTime = EventTxn#interdc_txn.timestamp,
                EventSnapshot = EventTxn#interdc_txn.snapshot,
                NewDownstreamEvent = #downstream_event{
                    event_dc = EventDc,
                    event_node = EventNode,
                    event_original_dc = EventOriginalDc,
                    event_commit_time = EventCommitTime,
                    event_snapshot_time = EventSnapshot,
                    event_txns = EventTxns},
                State2 = comm_recorder:do_record(NewDownstreamEvent, State),
                ?DEBUG_LOG("get_downstream_event_data::recorded"),
                commander_booter ! TxnId,
                {0, State2};
            {ok, AckNum} when AckNum < PartNum ->
                ?DEBUG_LOG("get_downstream_event_data::in last match"),
                {AckNum+1, State}
        end,
    NewTxnAckNum = dict:store({TxnId, EventDc}, NewAckNum, TxnAckNum),
    NewState = NewState1#comm_state{txn_ack_num = NewTxnAckNum},
    {reply, ok, NewState};

handle_call({update_upstream_event_data, {Data}}, _From, State) ->
    %% TODO: Handle the case if the txn is aborted or there is an error (Data =/= {txnid, _})
%%    ?DEBUG_LOG(io_lib:format("Commander::::: update upstream event data ~p.", [Data])),
    {TxId, InterDcTxn, DCID, CommitTime, SnapshotTime, _Partition} = Data,

    TxnPartNum = State#comm_state.txn_partial_num,
%%    ?DEBUG_LOG(io_lib:format("TxnPartNum: ~p", [dict:to_list(TxnPartNum)])),
    PartNum = dict:fetch(TxId, TxnPartNum),
    TxnAckNum = State#comm_state.txn_ack_num,
    AckNum =
        case dict:find({TxId, DCID}, TxnAckNum) of
            {ok, N} -> N;
            error -> 0
        end,
    NewState6 =
        if
            AckNum == 0 ->
                UpEvents = State#comm_state.upstream_events,
                DepClockPrgm = State#comm_state.dep_clock_prgm, %% (none, [{st, DepClockPrgm}, {ct, unknown}]),
                case UpEvents of
                    [CurrentUpstreamEvent | Tail] ->
                        NewEventTxns = CurrentUpstreamEvent#upstream_event.event_txns ++ [TxId],
                        NewUpstreamEvent = CurrentUpstreamEvent#upstream_event{event_dc = DCID, event_commit_time = CommitTime, event_snapshot_time = SnapshotTime, event_txns = NewEventTxns},

                        NewState1 = comm_recorder:do_record(NewUpstreamEvent, State),

                        NewTxnsData = dict:store(TxId, [{local, NewUpstreamEvent#upstream_event.event_data}, {remote, []}], NewState1#comm_state.txns_data),

                        {ok, Val} = dict:find(none, DepClockPrgm),
                        %% Sanity check
                        F = dict:filter(fun(K, _V) -> K == none end, DepClockPrgm),
                        1 = dict:size(F),
                        DepClockPrgm1 = dict:erase(none, DepClockPrgm),
                        %% Sanity check
                        unknown = proplists:get_value(ct, Val),

                        STPrgm = proplists:get_value(st, Val),
                        NewSTPrgm =
                            case STPrgm of
                                ignore ->
                                    dict:new();
                                _Else ->
                                    STPrgm
                            end,
                        Val1 = lists:keyreplace(st, 1, Val, {st, NewSTPrgm}),
                        VC_CT = dict:store(DCID, CommitTime, NewSTPrgm),
                        NewVal = lists:keyreplace(ct, 1, Val1, {ct, VC_CT}),
                        NewDepClockPrgm = dict:store(TxId, NewVal, DepClockPrgm1),

                        NewUpstreamEvents = Tail,
    %%                            ?DEBUG_LOG(io_lib:format("NewTxnData: ~p", [dict:to_list(NewTxnsData)])),
                        NewState1#comm_state{upstream_events = NewUpstreamEvents, txns_data = NewTxnsData, dep_clock_prgm = NewDepClockPrgm};
                    [] ->
                        throw("Upstream event client data does not exist!")
                end;
            true ->
                State
        end,
    ?DEBUG_LOG(io_lib:format("PartNum: ~p; AckNum: ~p", [PartNum, AckNum])),
    {NewAckNum, NewState5} =
        if
            AckNum + 1 == PartNum ->
                NewState3 = update_transactions_data(TxId, InterDcTxn, NewState6),
                commander_booter ! TxId,
                {0, NewState3};
            AckNum < PartNum ->
                NewState2 = update_transactions_data(TxId, InterDcTxn, NewState6),
                {AckNum + 1, NewState2}
        end,
    NewTxnAckNum = dict:store({TxId, DCID}, NewAckNum, TxnAckNum),
    NewState = NewState5#comm_state{txn_ack_num = NewTxnAckNum},
    {reply, ok, NewState};

handle_call({get_upstream_event_data, {Data}}, _From, State) ->
%%    ?DEBUG_LOG(io_lib:format("Commander:::::Get upstream event data to record: ~p.", [Data])),
    ?DEBUG_LOG("Commander:::::Get upstream event data to record."),
    {_M, [EvNo | Tail ]} = Data,
    [EventNode, ClockPrgm, _] = Tail,
    NewUpstreamEvent = #upstream_event{event_no = EvNo, event_node = EventNode, event_data = Data, event_txns = []},
    DepClockPrgm = State#comm_state.dep_clock_prgm,
    NewDepClockPrgm = dict:store(none, [{st, ClockPrgm}, {ct, unknown}], DepClockPrgm),
    NewUpstreamEvents = State#comm_state.upstream_events ++ [NewUpstreamEvent],

    NewState = State#comm_state{upstream_events = NewUpstreamEvents, dep_clock_prgm = NewDepClockPrgm},
    {reply, ok, NewState};

handle_call({set_ct_config, {Config}}, _From, State) ->
    NewState = State#comm_state{ct_config = Config},
    {reply, ok, NewState};

handle_call(get_ct_config, _From, State) ->
    CtConfig = State#comm_state.ct_config,
    {reply, CtConfig, State}.

handle_cast({check,{SchParam, Bound}}, State) ->
  {execution, 1, OrigSch} = State#comm_state.initial_exec,
  Scheduler = State#comm_state.scheduler,
  DelayDirection = State#comm_state.delay_direction,
  ok = write_time(Scheduler, starting),

  %%% Extract transactions dependency
  NewDepTxnsPrgm = extract_txns_dependency(State#comm_state.dep_clock_prgm),

  %%% for debugging
%%  KeysDepClock1 = dict:fetch_keys(State#comm_state.dep_clock_prgm),
%%  lists:foreach(fun(T) ->
%%                  {ok, Deps2} = dict:find(T, NewDepTxnsPrgm),
%%                  ?DEBUG_LOG(io_lib:format("~n ==--==--==--== For T: ~w; Txn Deps: ~w ==--==--==--==~n", [T, Deps2]))
%%                end, KeysDepClock1),

  %%% DCs list is obtained dynamically
  Clusters = State#comm_state.clusters,
%%  ?DEBUG_LOG(io_lib:format("commander::check::Clusters: ~p", [Clusters])),
  DCs = comm_utilities:get_all_dcs(Clusters),
%%  ?DEBUG_LOG(io_lib:format("commander::check::DCs: ~p", [DCs])),

  OrigSymSch = comm_utilities:get_det_sym_sch(OrigSch),
  TxnsData = State#comm_state.txns_data,
  comm_replayer:start_link(Scheduler, DelayDirection, SchParam, Bound, TxnsData, NewDepTxnsPrgm, Clusters, DCs, OrigSymSch),
  NewState = State#comm_state{phase = init_test, dep_txns_prgm = NewDepTxnsPrgm},
  comm_replayer:setup_next_test1(),
  {noreply, NewState};

handle_cast(test_initialized, State) ->
  NewState = State#comm_state{phase = replay, txn_partial_num = dict:new(), txn_ack_num = dict:new()},
  comm_replayer:replay_next_async(),
  {noreply, NewState};

handle_cast(run_next_test1, State) ->
  comm_replayer:setup_next_test1(),
  {noreply, State};

handle_cast({update_replay_txns_data, {LocalTxnData, InterDCTxn, TxId}}, State) ->
  ok = comm_replayer:update_txns_data(LocalTxnData, InterDCTxn, TxId),

    {_, _, DCID, _, _, _} = LocalTxnData,
    TxnPartNum = State#comm_state.txn_partial_num,
    PartNum = dict:fetch(TxId, TxnPartNum),

    TxnAckNum = State#comm_state.txn_ack_num,

    AckNum =
        case dict:find({TxId, DCID}, TxnAckNum) of
            {ok, N} -> N;
            error -> 0
        end,
    ?DEBUG_LOG(io_lib:format("commander::update_replay_txns_data; AckNum1: ~p", [AckNum])),

    NewAckNum =
        if
            AckNum+1 == PartNum ->
                Scheduler = State#comm_state.scheduler,
                %%% Check application invariant
                CurrSch = Scheduler:curr_schedule(),
                LatestEvent = lists:last(CurrSch),
                Serv_Cli = State#comm_state.server_client,
                TestRes = comm_verifier:check_object_invariant(LatestEvent, Serv_Cli),
                ?DEBUG_LOG(io_lib:format("commander::update_replay_txns_data; TestRes: ~p", [TestRes])),
                %%% If test result is true continue exploring more schedules; otherwise provide a counter example
                case TestRes of
                    true ->
                        comm_replayer:replay_next_async();
                    {caught, Exception, Reason} ->
                        display_counter_example(Scheduler, Exception, Reason)
                end,
                0;
            AckNum < PartNum ->
                AckNum + 1
        end,

    NewTxnAckNum = dict:store({TxId, DCID}, NewAckNum, TxnAckNum),
    NewState = State#comm_state{txn_ack_num = NewTxnAckNum},
  {noreply, NewState};

handle_cast({acknowledge_delivery, {TxId, RecDC, _Timestamp}}, State) ->
    ?DEBUG_LOG("in acknowledge_delivery"),
    TxnPartNum = State#comm_state.txn_partial_num,
    PartNum = dict:fetch(TxId, TxnPartNum),
    TxnAckNum = State#comm_state.txn_ack_num,

    AckNum =
        case dict:find({TxId, RecDC}, TxnAckNum) of
            {ok, N} -> N;
            error -> 0
        end,
    NewAckNum =
        if
            AckNum+1 == PartNum ->
                Scheduler = State#comm_state.scheduler,
                %%% Check application invariant
                CurrSch = Scheduler:curr_schedule(),
                LatestEvent = lists:last(CurrSch),

                ct:sleep(1000),
                Serv_Cli = State#comm_state.server_client,
                ?DEBUG_LOG("in ackn before check inv"),
                TestRes = comm_verifier:check_object_invariant(LatestEvent, Serv_Cli),
                ?DEBUG_LOG(io_lib:format("TestRes in ackn: ~p", [TestRes])),
                case TestRes of
                    true ->
                        comm_replayer:replay_next_async();
                    {caught, Exception, Reason} ->
                        display_counter_example(Scheduler, Exception, Reason)
                end,
                0;
            AckNum < PartNum ->
                AckNum + 1
        end,

    NewTxnAckNum = dict:store({TxId, RecDC}, NewAckNum, TxnAckNum),
    NewState = State#comm_state{txn_ack_num = NewTxnAckNum},
    {noreply, NewState};

handle_cast(stop, State) ->
    {stop, normal, State}.

handle_info(_Info, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

%%%====================================
%%% Internal functions
%%%====================================
update_transactions_data(TxId, InterDcTxn, State) ->
    ?DEBUG_LOG("Commander:::::Received back transaction data to update."),
    TxnsData = State#comm_state.txns_data,
%%    ?DEBUG_LOG(io_lib:format("TxnData: ~p", [dict:to_list(TxnsData)])),
%%    ?DEBUG_LOG(io_lib:format("InterDCTxnData: ~p", [InterDcTxn])),
    case dict:find(TxId, TxnsData) of
        {ok, TxData} ->
            [LocalData, {remote, PartTxns}] = TxData,
            NewPartTxns = PartTxns ++ [InterDcTxn],
            NewTxData = [LocalData, {remote, NewPartTxns}],
            NewTxnsData2 = dict:store(TxId, NewTxData, State#comm_state.txns_data),
%%            ?DEBUG_LOG(io_lib:format("NewTxnData: ~p", [dict:to_list(NewTxnsData2)])),
            State#comm_state{txns_data = NewTxnsData2};
        error ->
            ?DEBUG_LOG(io_lib:format("TxnData: ~p", [dict:to_list(TxnsData)])),
            ct:print("TXN:~p not found in txnsData!", [TxId]),
            State
        end.

display_check_result(Scheduler) ->
  ok = comm_utilities:write_to_file("comm_result",
      "===========================Verification Result===========================", append),
  ok = comm_utilities:write_to_file("comm_result",
      io_lib:format("~nChecking completed after exploring ~p schedules.~n",
          [Scheduler:schedule_count()]), append),
  ct:print("===========================Verification Result===========================
        Checking completed after exploring ~p schedules.~n", [Scheduler:schedule_count()]),
  commander_booter ! stop.

display_counter_example(Scheduler, Exception, Reason) ->
  ok = comm_utilities:write_to_file("comm_result",
    "===========================Verification Result===========================", append),
  ok = comm_utilities:write_to_file("comm_result",
    io_lib:format("~nChecking failed after exploring ~p schedules, by exception: ~p~nWith reason: ~p",
      [Scheduler:schedule_count(), Exception, Reason]), append),
  ct:print("===========================Verification Result==========================="),
  ct:print("Checking failed after exploring ~p schedules, by exception: ~p~nWith reason: ~p~n",
      [Scheduler:schedule_count(), Exception, Reason]),
  if
    Scheduler == comm_delay_scheduler ->
      ok = comm_utilities:write_to_file("comm_result",
        io_lib:format("Delay sequence: ~s~n", [Scheduler:get_delay_sequence()]), append),
      io:format("Delay sequence: "),
      Scheduler:print_delay_sequence();
    true ->
      noop
  end,
  ok = comm_utilities:write_to_file("counter_example",
    "===========================Counter Example===========================", append),
  CounterExample = Scheduler:curr_schedule(),
  ok = comm_utilities:write_to_file("counter_example",
    io_lib:format("~n~w~nCE length: ~p~n", [CounterExample, length(CounterExample)]), append),
  ct:print("===========================Counter Example==========================="),
  ct:print("~w", [CounterExample]),
  ct:print("Counter example length (written to commanderDir/schedules/TestName): ~p", [length(CounterExample)]),
  ?DEBUG_LOG(io_lib:format("~nCounter example length (written to commanderDir/schedules/TestName): ~p~n",
      [length(CounterExample)])),
%%  write_time(Scheduler, ending).,
  commander_booter ! stop.

%%% Extract transactions dependency
extract_txns_dependency(DepClockPrgm) ->
%%  print_dict_of_dict(DepClockPrgm),
  NewDepTxnsPrgm =
    dict:fold(fun(TxId, [{st, ST}, {ct, _CT}], AllDepTxns) ->
                case dict:size(ST) of
                  0 ->
                    D1 = dict:store(TxId, [], AllDepTxns),
                    D1;
                  _Else ->
                    KeysDepClock = dict:fetch_keys(DepClockPrgm),
                    Dependencies =
                    lists:foldl(fun(T, DepTxns) ->
                                  {ok, [{st, _T_ST}, {ct, T_CT}]} = dict:find(T, DepClockPrgm),
                                  case (TxId /= T) and vectorclock:ge(ST, T_CT) of
                                    true ->
                                      DepTxns ++ [T];
                                    false ->
                                      DepTxns
                                  end
                                end, [], KeysDepClock),
                    D2 = dict:store(TxId, Dependencies, AllDepTxns),
                    D2
                end
              end, dict:new(), DepClockPrgm),

%%  print_dict(NewDepTxnsPrgm),
  NewDepTxnsPrgm.

%%print_dict(D) ->
%%  Keys = dict:fetch_keys(D),
%%  lists:foreach(fun(Key) ->
%%                  {ok, KDeps} = dict:find(Key, D),
%%                  ?DEBUG_LOG(io_lib:format("~n~n!!!!!!! K: ~w; KDeps: ~w !!!!!!!~n~n", [Key, KDeps]))
%%                end, Keys).

%%print_dict_of_dict(D) ->
%%  Keys = dict:fetch_keys(D),
%%  lists:foreach(fun(Key) ->
%%                  {ok, KDeps} = dict:find(Key, D),
%%                  [{st, ST}, {ct, CT}] = KDeps,
%%                  STLst = dict:to_list(ST),
%%                  CTLst = dict:to_list(CT),
%%                  ?DEBUG_LOG(io_lib:format("~n#########Txn: ~w ### ~nST: ~w~n CT: ~w #########~n", [Key, STLst, CTLst]))
%%                end, Keys).

write_time(_Scheduler, P) -> %% P: starting | ending
  comm_utilities:write_to_file("comm_result", io_lib:format("~n~w:~w~n", [P, erlang:localtime()]), anything).