%%%-------------------------------------------------------------------
%%% @author olevchenko
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(ratelimiter_sup).

-behaviour(supervisor).

-export([start_link/0, init/1]).

-define(SERVER, ?MODULE).
-define(STOP_CHILD_TIMEOUT, 2000).
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, ?STOP_CHILD_TIMEOUT, Type, []}).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Supflags = #{
        strategy  => one_for_one,
        intensity => 1000,
        period    => 1
    },
    BucketStorage = ?CHILD(tocken_bucket, worker),
    Workers = [BucketStorage],
    {ok, {Supflags, Workers}}.