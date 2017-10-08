-module(comm_consistency).

-include("commander.hrl").
-include_lib("eunit/include/eunit.hrl").

%% API
-export([is_runnable/9,
    is_blocked/7]).

is_runnable(cc, local, CurrEvent, CurrSch, _Delayed, _SS, CurrEventIndex, DelIndex, AllDepTxnsPrgm) ->
    IsDelaying = (CurrEventIndex == DelIndex),

    EventDC = CurrEvent#local_event.event_dc,
    [EventTxnId] = CurrEvent#local_event.event_txns,

    {ok, EventDepTxns} = dict:find(EventTxnId, AllDepTxnsPrgm),

    DepSatisfied =
        lists:all(fun(DepT) ->
            lists:any(fun(E) ->
                Type = comm_utilities:type(E),
                {[ETxId], EDC} =
                    case Type of
                        local ->
                            {E#local_event.event_txns, E#local_event.event_dc};
                        remote ->
                            {E#remote_event.event_txns, E#remote_event.event_dc}
                    end,
                ETxId == DepT andalso EDC == EventDC
                      end, CurrSch)
                  end, EventDepTxns),
    not IsDelaying and DepSatisfied;

is_runnable(cc, remote, CurrEvent, _CurrSch, Delayed, SS, CurrEventIndex, DelIndex, _AllDepTxnsPrgm) ->

    IsDelaying = (CurrEventIndex == DelIndex),

    EventDC = CurrEvent#remote_event.event_dc,
    {ok, DCSS} = dict:find(EventDC, SS),
    DepSatisfied =  vectorclock:ge(DCSS, CurrEvent#remote_event.event_snapshot_time),

    %%% Check if CurrEvent is replication of a delayed event
    IsRepl =
        lists:any(fun(E) ->
            case comm_utilities:type(E) of
                local ->
                    CurrEvent#remote_event.event_txns == E#local_event.event_txns;
                _ ->
                    false
            end
                  end, Delayed),
    (not IsDelaying) andalso (not IsRepl) andalso DepSatisfied;

is_runnable(sr, local, CurrEvent, CurrSch, _Delayed, SS, CurrEventIndex, DelIndex, AllDepTxnsPrgm) ->
    IsDelaying = (CurrEventIndex == DelIndex),

    EventDC = CurrEvent#local_event.event_dc,
    {ok, DCSS} = dict:find(EventDC, SS),
    [EventTxnId] = CurrEvent#local_event.event_txns,

    {ok, EventDepTxns} = dict:find(EventTxnId, AllDepTxnsPrgm),

    DepSatisfied =
        lists:all(fun(DepT) ->
            lists:any(fun(E) ->
                Type = comm_utilities:type(E),
                {[ETxId], EDC} =
                    case Type of
                        local ->
                            {E#local_event.event_txns, E#local_event.event_dc};
                        remote ->
                            {E#remote_event.event_txns, E#remote_event.event_dc}
                    end,
                ETxId == DepT andalso EDC == EventDC
                      end, CurrSch)
                  end, EventDepTxns),

    %% ensure serializability
    SRDepClock = dict:fold(fun(_DC, VCClk, Acc) ->
                                vectorclock:max([VCClk, Acc])
                            end, dict:new(), SS),

    SRSatisfied = vectorclock:eq(DCSS, SRDepClock),

    SRSatisfied and not IsDelaying and DepSatisfied;

is_runnable(sr, remote, CurrEvent, _CurrSch, Delayed, SS, CurrEventIndex, DelIndex, _AllDepTxnsPrgm) ->

    IsDelaying = (CurrEventIndex == DelIndex),

    EventDC = CurrEvent#remote_event.event_dc,
    {ok, DCSS} = dict:find(EventDC, SS),
    DepSatisfied =  vectorclock:eq(DCSS, CurrEvent#remote_event.event_snapshot_time),

    %%% Check if CurrEvent is replication of a delayed event
    IsRepl =
        lists:any(fun(E) ->
            case comm_utilities:type(E) of
                local ->
                    CurrEvent#remote_event.event_txns == E#local_event.event_txns;
                _ ->
                    false
            end
                  end, Delayed),
    (not IsDelaying) andalso (not IsRepl) andalso DepSatisfied.

is_blocked(cc, local, Event, _SS, _CurrSch, Remained, AllDepTxnsPrgm) ->
    EventDC = Event#local_event.event_dc,
    [EventTxnId] = Event#local_event.event_txns,

    {ok, EventDepTxns} = dict:find(EventTxnId, AllDepTxnsPrgm),
%%  io:format("~n==========EventDepTxns: ~w~n===========", [EventDepTxns]),
    Res =
        lists:any(fun(DepT) ->
            InnerRes =
                lists:any(fun(RemEv) ->
                    Type = comm_utilities:type(RemEv),
%%                          io:format("~n======RemEvType: ~w ~n======", [Type]),
                    {[RemEvTxnId], RemEvDC} =
                        case Type of
                            local -> {RemEv#local_event.event_txns, RemEv#local_event.event_dc};
                            remote -> {RemEv#remote_event.event_txns, RemEv#remote_event.event_dc}
%%                              ok -> io:format("~n======RemEv: ~w ~n======", [RemEv])
                        end,
%%                          io:format("~n ======== DepT id is remained: ~w ; DepT DC is the same: ~w =========", [RemEvTxnId == DepT, RemEvDC == EventDC]),
%%                          io:format("~n ========DepT: ~w ========~n Event: ~w ===~n RemEvent: ~w ~n=========", [DepT, Event, RemEv]),
                    RemEvTxnId == DepT andalso RemEvDC == EventDC
                          end, Remained),
%%              io:format("~n ======== Inner is blocked: ~w =========", [InnerRes]),
            InnerRes
                  end, EventDepTxns),
%%  io:format("~n ======== local is blocked: ~w =========", [Res]),
    Res;

%%% Returns true if Event is
%%% either the replication of an event which has not been replayed
%%% or its dependency is not satisfied
is_blocked(cc, remote, Event, SS, CurrSch, Remained, _AllDepTxnsPrgm) ->
    %%% Check if it is the replication of an unscheduled local event
    Possible =
        not lists:any(fun(E) ->
            case comm_utilities:type(E) of
                local ->
                    Event#remote_event.event_txns == E#local_event.event_txns;
                _ ->
                    false
            end end, Remained),

    %%% Sanity check
    Possible = lists:any(fun(E) ->
        case comm_utilities:type(E) of
            local ->
                Event#remote_event.event_txns == E#local_event.event_txns;
            _ ->
                false
        end end, CurrSch),

    %%% If scheduling Event is possible, check if its dependency is satisfied
    case Possible of
        true ->
            EventDC = Event#remote_event.event_dc,
            {ok, DC_SS} = dict:find(EventDC, SS),
            not vectorclock:ge(DC_SS, Event#remote_event.event_snapshot_time);
        false ->
            true
    end;

is_blocked(sr, local, Event, SS, _CurrSch, Remained, AllDepTxnsPrgm) ->
    EventDC = Event#local_event.event_dc,
    [EventTxnId] = Event#local_event.event_txns,

    {ok, EventDepTxns} = dict:find(EventTxnId, AllDepTxnsPrgm),
%%  io:format("~n==========EventDepTxns: ~w~n===========", [EventDepTxns]),
    Res = lists:any(fun(DepT) ->
                    InnerRes =
                        lists:any(fun(RemEv) ->
                                    Type = comm_utilities:type(RemEv),
                                    {[RemEvTxnId], RemEvDC} =
                                        case Type of
                                            local -> {RemEv#local_event.event_txns, RemEv#local_event.event_dc};
                                            remote -> {RemEv#remote_event.event_txns, RemEv#remote_event.event_dc}
                                        end,
                                    RemEvTxnId == DepT andalso RemEvDC == EventDC
                                  end, Remained),
                    InnerRes
                    end, EventDepTxns),

    %% ensure serializability
    SRDepClock = dict:fold(fun(_DC, VCClk, Acc) ->
                            vectorclock:max([VCClk, Acc])
                           end, dict:new(), SS),

    {ok, DCSS} = dict:find(EventDC, SS),
    SRSatisfied = vectorclock:eq(DCSS, SRDepClock),

    not SRSatisfied or Res;

is_blocked(sr, remote, Event, SS, CurrSch, Remained, _AllDepTxnsPrgm) ->
    %%% Check if it is the replication of an unscheduled local event
    Possible =
        not lists:any(fun(E) ->
            case comm_utilities:type(E) of
                local ->
                    Event#remote_event.event_txns == E#local_event.event_txns;
                _ ->
                    false
            end end, Remained),

    %%% Sanity check
    Possible = lists:any(fun(E) ->
        case comm_utilities:type(E) of
            local ->
                Event#remote_event.event_txns == E#local_event.event_txns;
            _ ->
                false
        end end, CurrSch),

    %%% If scheduling Event is possible, check if its dependency is satisfied
    case Possible of
        true ->
            EventDC = Event#remote_event.event_dc,
            {ok, DC_SS} = dict:find(EventDC, SS),
            not vectorclock:eq(DC_SS, Event#remote_event.event_snapshot_time);
        false ->
            true
    end.