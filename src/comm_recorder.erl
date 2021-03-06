-module(comm_recorder).

-include("../include/commander.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([do_record/2, init_record/2]).

%%%====================================
%%% Public API
%%%====================================
init_record(ExecId, State) ->
    CurrExec = #execution{id=ExecId, trace=[]},
    if
        ExecId == 1 ->
            NewState = State#comm_state{initial_exec = CurrExec, curr_exec = CurrExec, curr_delay_seq=[], replay_history=[], phase=record, exec_counter = ExecId, upstream_events = [], txns_data = dict:new(), dep_clock_prgm = dict:new(), dep_txns_prgm = dict:new()};
        true ->
            NewState = State
    end,
    FileName = comm_utilities:get_exec_name(ExecId),
    Phase = NewState#comm_state.phase,
    FullName = comm_utilities:get_full_name(FileName, Phase),
    write_to_file(FullName, io_lib:format("~b~n", [ExecId]), write),
    NewState.

do_record(Event, State) ->
    ExecId = State#comm_state.exec_counter,
    FileName = comm_utilities:get_exec_name(ExecId),
    Phase = State#comm_state.phase,
    FullName = comm_utilities:get_full_name(FileName, Phase),
    ok = write_to_file(FullName, io_lib:format("~w~n", [Event]), append),

    %% Update common parts of the commander state for both upstream and downstream events
    CurrExec = State#comm_state.curr_exec,
    CurrExecTrace = CurrExec#execution.trace,
    NewCurrExecTrace = CurrExecTrace ++ [Event],
    NewCurrExec = CurrExec#execution{trace = NewCurrExecTrace},
    if
        ExecId == 1 ->
            NewState = State#comm_state{initial_exec = NewCurrExec, curr_exec = NewCurrExec};
        true ->
            NewState = State#comm_state{curr_exec = NewCurrExec}
    end,
    NewState.

%%%====================================
%%% Internal functions
%%%====================================
write_to_file(FullName, Data, Mode) ->
    ?DEBUG_LOG(io_lib:format("comm_recorder::Fullname: ~s", [FullName])),
    case file:read_file_info(FullName) of
            {ok, _FileInfo} ->
                ?DEBUG_LOG("recorder::read file successfully!"),
                file:write_file(FullName, Data, [Mode]),
                ok;
            {error, enoent} -> file:write_file(FullName, Data, [Mode]);
            {error, Reason} ->
                ?DEBUG_LOG(io_lib:format("recorder::read file failed: ~p", [Reason])),
                throw(io_lib:format("Exception: ~p", [{error, Reason}]))
    end.