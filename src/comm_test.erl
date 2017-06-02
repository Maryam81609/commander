-module(comm_test).

-include("../include/commander.hrl").

-export([event/2, objects/2]).

-callback check(Config :: list()) ->
    pass | {error, Reason :: string()}.
-callback handle_event(Args :: list(term())) ->
    Result :: term() | {error, Reason :: string()}.
-callback handle_object_invariant(Node :: node(), Objs :: list(term)) ->
    Result :: true | {error, Reason :: string()}.

-spec(event(Module :: atom(), Args :: list(term())) -> term()).
event(Module, Args) ->
    Data = {Module, Args},
    timer:sleep(100),
    ok = commander:get_upstream_event_data(Data),
    Res = Module:handle_event(Args),
    Res.

-spec(objects(Module :: atom(), Objs::list()) -> ok).
objects(Module, Objs) ->
    ok = commander:get_app_objects(Module, Objs),
    ok.