%%%-------------------------------------------------------------------
%%% @author olevchenko
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 02. лис 2022 13:10
%%%-------------------------------------------------------------------
-module(ratelimiter).
-author("olevchenko").

%% API
-export([start/0, stop/0]).

start() ->
    {ok, _} = application:ensure_all_started(ratelimiter, permanent).

stop() ->
    application:stop(ratelimiter).
