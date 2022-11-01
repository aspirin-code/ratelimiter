%%%-------------------------------------------------------------------
%%% @author olevchenko
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(tocken_bucket_storage).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).
-define(CLEAR_INTERVAL, 60000). %% ms
-define(MAX_RPS, 3). %% sec

-record(tocken_bucket_state, {timer_ref}).
-record(tocken, {
    id = {erlang:system_time(seconds), undefined},
    req_count = 0
}).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    ets:new(?MODULE, [private, ordered_set, named_table, {read_concurrency, true}, {keypos, #tocken.id}]),
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
    NextKey = ets:next(?MODULE, FirstKey),
    case ets:lookup_element(?MODULE, FirstKey, 2) of
        ?MAX_RPS ->
            ets:delete(?MODULE, FirstKey);
        _ ->
            ignore
    end,
    clear_old(NextKey).
