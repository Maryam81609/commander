-module(comm_utilities).

-include("../include/commander.hrl").

-compile(export_all).

-undef(DEBUG).
-define(DEBUG, false).

get_home_dir() ->
    {ok, [[HomeDir]]} = init:get_argument(home),
    HomeDir.

is_initial_exec(History) when is_list(History) ->
    case History of
        [] -> true;
        _ -> false
    end.

get_exec_name(_ExecId) ->
    "orig_exec".
%%    FileName = io_lib:format("exec_~b",[ExecId]),
%%    FileName.

get_full_name(RelPath, Phase) ->
    SUTName = os:getenv("TEST"),
    ?DEBUG_LOG(io_lib:format("comm_utilities::SUTName: ~s", [SUTName])),
    CommDir = os:getenv("COMMDIR"),
    ?DEBUG_LOG(io_lib:format("comm_utilities::CommDir: ~s", [CommDir])),
    RootDir = filename:join([CommDir, "schedules", SUTName]),
    ?DEBUG_LOG(io_lib:format("comm_utilities::RootDir: ~s", [RootDir])),
    AbsPath =
        case Phase of
            record ->
                P = filename:join([RootDir, RelPath]),
                ?DEBUG_LOG(io_lib:format("comm_utilities::AbsPath: ~s", [P])),
                P
%%            replay -> filename:join([RootDir, RelPath])
        end,
    ok = filelib:ensure_dir(AbsPath),
%%    FullName = Dir ++ Name,
%%    ok = filelib:ensure_dir(FullName),
    AbsPath.

get_symbolic_sch(OrigSch) ->
    TrimedSch = lists:map(fun(E) -> trim(E) end, OrigSch),
    RevTrimedSch = lists:reverse(TrimedSch),
    lists:reverse(remove_dups(RevTrimedSch)).

%%%===== Internals
remove_dups([]) -> [];
remove_dups([H | T]) -> [H | [E || E <- remove_dups(T), not_equal(H, E)]].

not_equal(E1, E2) ->
    if
        (is_record(E1, remote_event) and is_record(E2, remote_event)) ->
            not ((E1#remote_event.event_dc == E2#remote_event.event_dc) and (E1#remote_event.event_txns == E2#remote_event.event_txns));
        true ->
            true
    end.

trim(Event) ->
    if
        is_record(Event, upstream_event) -> trim(local_event, Event);
        is_record(Event, downstream_event) -> trim(remote_event, Event)
    end.

trim(local_event, Event) ->
    EvNo = Event#upstream_event.event_no,
    EvNode = Event#upstream_event.event_node,
    EvDc = Event#upstream_event.event_dc,
    EvCT = Event#upstream_event.event_commit_time,
    EvST = Event#upstream_event.event_snapshot_time,
    EvTxns = Event#upstream_event.event_txns,
    #local_event{event_no = EvNo, event_node = EvNode, event_dc = EvDc, event_commit_time = EvCT, event_snapshot_time = EvST, event_txns =EvTxns};

trim(remote_event, Event) ->
    EvDc = Event#downstream_event.event_dc,
    EvNode = Event#downstream_event.event_node,
    EvOrigDc = Event#downstream_event.event_original_dc,
    EvCT = Event#downstream_event.event_commit_time,
    EvST = Event#downstream_event.event_snapshot_time,
    EvTxns = Event#downstream_event.event_txns,
    #remote_event{event_dc = EvDc, event_node = EvNode, event_original_dc = EvOrigDc, event_commit_time = EvCT, event_snapshot_time = EvST, event_txns = EvTxns}.

reset_dcs(Clusters) ->
    io:format("~nReseting test environment...~nCluaters: ~p~n", [Clusters]),
    Clean = rt_config:get(clean_cluster, true),
    Clusters1 = common:clean_clusters(Clusters),
    ok = common:setup_dc_manager(Clusters1, Clean),
    Clusters1.

type(Event) ->
    if
        is_record(Event, local_event) -> local;
        is_record(Event, remote_event) -> remote
    end.

get_all_dcs(Clusters) ->
    lists:map(fun(Cluster) ->
                Node = hd(Cluster),
                rpc:call(Node, dc_utilities, get_my_dc_id, [])
              end, Clusters).

get_all_partitions(ReplayerState) ->
    Clusters = ReplayerState#replay_state.clusters,
    HeadNodes = lists:map(fun(Cluster) ->
                            hd(Cluster)
                          end, Clusters),

    lists:map(fun(HeadNode) ->
                rpc:call(HeadNode, dc_utilities, get_all_partitions, [])
              end, HeadNodes).

write_to_file(RelFileName, Data, Mode) ->
    FileName = get_full_path(RelFileName, replay),
    case Mode of
        anything ->
            case filelib:is_regular(FileName) of
                true ->
                    file:write_file(FileName, Data, [append]),
                    ok;
                false ->
                    file:write_file(FileName, Data, [write]),
                    ok
            end;
        Mode2 ->
            file:write_file(FileName, Data, [Mode2]),
            ok
    end.

get_det_sym_sch(OrigSch) ->
    SymbSch = get_symbolic_sch(OrigSch),
    {Locals, Remotes} =
        lists:foldl(fun(E, {L, R}) ->
                        case type(E) of
                            local ->
                                {L ++ [E], R};
                            remote ->
                                {L, R ++ [E]}
                        end
                    end, {[], []}, SymbSch),

    SortedLocals =
        lists:sort(fun(E1, E2) ->
                        E1DC = E1#local_event.event_dc,
                        [E1Tx] = E1#local_event.event_txns,
                        E2DC = E2#local_event.event_dc,
                        [E2Tx] = E2#local_event.event_txns,
                        E1DC =< E2DC andalso E1Tx =< E2Tx
                   end, Locals),

    SortedRemotes =
        lists:sort(fun(E1, E2) ->
                        E1DC = E1#remote_event.event_dc,
                        E1OrigDC = E1#remote_event.event_original_dc,
                        [E1Tx] = E1#remote_event.event_txns,
                        E2DC = E2#remote_event.event_dc,
                        E2OrigDC = E2#remote_event.event_original_dc,
                        [E2Tx] = E2#remote_event.event_txns,
                        E1DC =< E2DC andalso E1OrigDC =< E2OrigDC andalso E1Tx =< E2Tx
                   end, Remotes),
  SortedLocals ++ SortedRemotes.

get_full_path(RelPath, replay) ->
    CommDir = os:getenv("COMMDIR"),
    TestName = os:getenv("TEST"),
    SchedulerName = os:getenv("SCHEDULER"),
    FullPath = filename:join([CommDir, "schedules", TestName, SchedulerName, RelPath]),
    ?DEBUG_LOG(io_lib:format("Write Full Path: ~p", [FullPath])),
    filelib:ensure_dir(FullPath),
    FullPath.