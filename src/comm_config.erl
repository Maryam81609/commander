%%%-------------------------------------------------------------------
%%% @author maryam
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. Jun 2017 3:09 PM
%%%-------------------------------------------------------------------
-module(comm_config).
-author("maryam").

%% API
-export([init/1,
    get/1]).

init(Data) ->
    ConfFilename = configFilename(),
    ct:print(ConfFilename),
    case filelib:is_regular(ConfFilename) of
        true ->
            skip;
        false ->
            ConfData = io_lib:format("~p.", [Data]),
            ok = file:write_file(ConfFilename, ConfData, [write])
    end.

get(Key) ->
    get(Key, commander).

get(ParamKey, App) ->
    ConfFilename = configFilename(),
    case file:consult(ConfFilename) of
        {ok, ConfData} ->
            AppConfig = proplists:get_value(App, ConfData),
            proplists:get_value(ParamKey, AppConfig);
        {error, Reason} ->
            throw(io_lib:format("Reading commander.config failed with reason: ~p", [Reason]))
    end.

configFilename() ->
    CommDir = os:getenv("COMMDIR"),
    filename:join([CommDir, "commander.config"]).