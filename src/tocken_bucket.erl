%%%-------------------------------------------------------------------
%%% @author olevchenko
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 01. лис 2022 15:35
%%%-------------------------------------------------------------------
-module(tocken_bucket).
-author("olevchenko").

%% API
-export([start/0, stop/0]).


start() ->
    {ok, _} = application:ensure_all_started(tocken_bucket, permanent).

stop() ->
    Res = application:stop(tocken_bucket),
    application:stop(crypto),
    Res.