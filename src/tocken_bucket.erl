%%%-------------------------------------------------------------------
%%% @author olevchenko
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(tocken_bucket).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3]).
-export([
    is_limit_reached/1,
    is_limit_reached/2
]).

-define(SERVER, ?MODULE).
-define(CLEAR_INTERVAL, 60000). %% ms
-define(MAX_RPS, 3). %% sec

-type user_id() :: non_neg_integer().
-type max_rps() :: non_neg_integer().

-record(tocken_bucket_state, {timer_ref}).
-record(tocken, {
    key :: term(),
    req_count = 0
}).

%%====================================================
%%API
%%===================================================

-spec is_limit_reached(user_id()) -> boolean().
is_limit_reached(UserId) when is_integer(UserId) andalso UserId > 0 ->
    is_limit_reached(UserId, ?MAX_RPS);
is_limit_reached(UserId) ->
    io:format("Not valid UserId: ~p", [UserId]),
    throw('not_valid_userid').

-spec is_limit_reached(user_id(), max_rps()) -> boolean().
is_limit_reached(UserId, MaxRps) ->
    Timestamp = os:system_time(seconds),
    Key ={UserId, Timestamp},
    case ets:member(?MODULE, Key) of
        false ->
            ets:insert(?MODULE, #tocken{key = Key, req_count = 1}),
            false;
        true ->
            Counter = ets:update_counter(?MODULE, Key, {3, 1}),
            Counter > MaxRps
    end.


%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    ?MODULE = ets:new(?MODULE, [public, ordered_set, named_table, {read_concurrency, true}, {keypos, #tocken.key}]),
    TimerRef = erlang:send_after(?CLEAR_INTERVAL, self(), clear_old),
    {ok, #tocken_bucket_state{timer_ref = TimerRef}}.

handle_call(_Request, _From, State = #tocken_bucket_state{}) ->
    {reply, ok, State}.

handle_cast(_Request, State = #tocken_bucket_state{}) ->
    {noreply, State}.

handle_info(clear_old, #tocken_bucket_state{timer_ref = OldTimerRef} = State) ->
    _ = erlang:cancel_timer(OldTimerRef),
    ok = clear_old(),
    TimerRef = erlang:send_after(?CLEAR_INTERVAL, self(), clear_old),
    io:format("Clear expired records"),
    {noreply, State#tocken_bucket_state{timer_ref = TimerRef}};
handle_info(_Info, State = #tocken_bucket_state{}) ->
    {noreply, State}.

terminate(_Reason, _State = #tocken_bucket_state{}) ->
    ok.

code_change(_OldVsn, State = #tocken_bucket_state{}, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

clear_old() ->
    FirstKey = ets:first(?MODULE),
    clear_old(FirstKey).

clear_old('$end_of_table') ->
    ok;
clear_old(FirstKey) ->
    Now = os:system_time(seconds),
    NextKey = ets:next(?MODULE, FirstKey),
    case ets:lookup_element(?MODULE, FirstKey, 2) of
        {_UserId, TTL} when TTL < Now ->
            ets:delete(?MODULE, FirstKey);
        _ ->
            ignore
    end,
    clear_old(NextKey).