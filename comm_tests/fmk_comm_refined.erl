-module(fmk_comm_refined).

-behavior(comm_test).

-include_lib("eunit/include/eunit.hrl").

%% comm_test callbacks
-export([check/1,
    handle_event/1,
    handle_object_invariant/2]).

%%% Event IDs
-define(CRT_PTNT_EV, 1).
-define(CRT_PRSC_EV, 2).
-define(CRT_PHRM_EV, 3).
-define(CRT_STFF_EV, 4).
-define(UPD_PTNT_EV, 5).

%%% DB Object IDs
-define(P1ID, 1).
-define(P2ID, 2).
-define(PHRM1ID, 3).
-define(PHRM2ID, 4).
-define(STFF1ID, 5).
-define(STFF2ID, 6).
-define(PRSC1ID, 7).
-define(PRSC2ID, 8).

%%% =============================
%%% comm_test callback functions
%%% =============================
check(Config) ->
    [Node1, Node2, Node3] = proplists:get_value(sut_nodes, Config),
    main_test([Node1, Node2, Node3]),
    pass.

handle_event([?CRT_PTNT_EV, Node, _Clock, Params]) ->
    case rpc:call(Node, fmk_core, create_patient, Params) of
        ok -> skip;
        Else ->
            ct:print("Exception: ~p---~p", [Else, Params]),
            throw("failed create patient!")
    end;

handle_event([?CRT_PRSC_EV, Node, _Clock, Params]) ->
    case rpc:call(Node, fmk_core, create_prescription, Params) of
        ok -> skip;
        Else ->
            ct:print("Exception: ~p", [Else]),
            throw("failed!")
    end;

handle_event([?CRT_PHRM_EV, Node, _Clock, Params]) ->
    ok = rpc:call(Node, fmk_core, create_pharmacy, Params);

handle_event([?CRT_STFF_EV, Node, _Clock, Params]) ->
    ok = rpc:call(Node, fmk_core, create_staff, Params);

handle_event([?UPD_PTNT_EV, Node, _Clock, Params]) ->
    ok = rpc:call(Node, fmk_core, update_patient_details, Params).

handle_object_invariant(Node, [PId1, PId2, Pharm1Id, Staff1Id, Presc1Id, Pharm2Id, Staff2Id, Presc2Id]) ->
    invariant_presc(Node, [PId1, PId2, Pharm1Id, Staff1Id, Presc1Id, Pharm2Id, Staff2Id, Presc2Id]),
    true.

%%% =============================
%%% Internal functions
%%% =============================
invariant_presc(Node, [PId1, PId2, Pharm1Id, Staff1Id, Presc1Id, Pharm2Id, Staff2Id, Presc2Id]) ->
    Presc1 = rpc:call(Node, fmk_core, get_prescription_by_id, [Presc1Id]),
    %%% If a prescription object exists in the DB,
    %%% it must be present in the patient, staff and pharmacy objects as well
    case Presc1 of
        {error, _} -> skip;
        Presc1 ->
            Patient1 = rpc:call(Node, fmk_core, get_patient_by_id, [PId1]),
            Pharmacy1 = rpc:call(Node, fmk_core, get_pharmacy_by_id, [Pharm1Id]),
            Staff1 = rpc:call(Node, fmk_core, get_staff_by_id, [Staff1Id]),
            ?assertMatch(patient, element(1, Patient1)),
            ?assertMatch(pharmacy, element(1, Pharmacy1)),
            ?assertMatch(staff, element(1, Staff1)),

            {_, _, _, _, Patient1Prescs} = Patient1,
            PrescUpd = setelement(3, Presc1, <<"undefined">>),
            ?assert(lists:member(PrescUpd, Patient1Prescs)),

            {_, _, _, _, PharmPrescs} = Pharmacy1,
            PrescUpd2 = setelement(4, Presc1, <<"undefined">>),
            ?assert(lists:member(PrescUpd2, PharmPrescs)),

            {_, _, _, _, _, StaffPrescs} = Staff1,
            PrescUpd3 = setelement(5, Presc1, <<"undefined">>),
            ?assert(lists:member(PrescUpd3, StaffPrescs))
    end,

    Presc2 = rpc:call(Node, fmk_core, get_prescription_by_id, [Presc2Id]),
    case Presc2 of
        {error, _} ->
            skip;
        _Else ->
            Patient2 = rpc:call(Node, fmk_core, get_patient_by_id, [PId2]),
            Pharmacy2 = rpc:call(Node, fmk_core, get_pharmacy_by_id, [Pharm2Id]),
            Staff2 = rpc:call(Node, fmk_core, get_staff_by_id, [Staff2Id]),
            ?assertMatch(patient, element(1, Patient2)),
            ?assertMatch(pharmacy, element(1, Pharmacy2)),
            ?assertMatch(staff, element(1, Staff2)),

            {_, _, _, _, Patient2Prescs} = Patient2,
            PrescUpd4 = setelement(3, Presc2, <<"undefined">>),
            ?assert(lists:member(PrescUpd4, Patient2Prescs)),

            {_, _, _, _, Pharm2Prescs} = Pharmacy2,
            PrescUpd5 = setelement(4, Presc2, <<"undefined">>),
            ?assert(lists:member(PrescUpd5, Pharm2Prescs)),

            {_, _, _, _, _, Staff2Prescs} = Staff2,
            PrescUpd6 = setelement(5, Presc2, <<"undefined">>),
            ?assert(lists:member(PrescUpd6, Staff2Prescs))
    end.

main_test([Node1, Node2, Node3]) ->
    comm_test:objects(?MODULE, [?P1ID, ?P2ID, ?PHRM1ID, ?STFF1ID, ?PRSC1ID, ?PHRM2ID, ?STFF2ID, ?PRSC2ID]),

    dc1_txns(Node1, {Node2, Node3}, [?P2ID]),
    dc2_txns(Node2, {Node1, Node3}, [?P1ID, ?PHRM1ID, ?STFF1ID, ?PRSC1ID]),
    dc3_txns(Node3, {Node1, Node2}, [?P1ID, ?P2ID, ?PHRM2ID, ?STFF2ID, ?PRSC2ID]).

dc1_txns(Node, {Node2, Node3}, [PId2]) ->
    comm_test:event(?MODULE, [?CRT_PTNT_EV, Node, [{Node, 1}, {Node2, 0}, {Node3, 0}],
        [PId2, "name2", "p-address2"]]).

dc2_txns(Node, {Node1, Node3}, [PId1, _PharmId, _StaffId, _PrescId]) ->
    comm_test:event(?MODULE, [?CRT_PTNT_EV, Node, [{Node, 1}, {Node1, 0}, {Node3, 0}],
        [PId1, "name1", "p-address1"]]).

dc3_txns(Node, {Node1, Node2}, [_PId1, PId2, Pharmc2Id, Staff2Id, Prsc2Id]) ->
    comm_test:event(?MODULE, [?UPD_PTNT_EV, Node, [{Node, 1}, {Node1, 0}, {Node2, 0}],
        [PId2, "name2-updd", "p2-add-updd"]]),
    comm_test:event(?MODULE, [?CRT_PHRM_EV, Node, [{Node, 1}, {Node1, 0}, {Node2, 0}],
        [Pharmc2Id, "pharmacy2", "ph-address2"]]),
    comm_test:event(?MODULE, [?CRT_STFF_EV, Node, [{Node, 1}, {Node1, 0}, {Node2, 0}],
        [Staff2Id, "staff2", "st-address2", "st2-specialty"]]),
    comm_test:event(?MODULE, [?CRT_PRSC_EV, Node, [{Node, 2}, {Node1, 0}, {Node2, 0}],
        [Prsc2Id, PId2, Staff2Id, Pharmc2Id, "07-17-17", ["drugs3"]]]).