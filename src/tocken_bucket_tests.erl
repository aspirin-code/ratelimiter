-module(tocken_bucket_tests).

-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

is_limit_reached_test() ->
    ?assertMatch(false, tocken_bucket:is_limit_reached(123)),
    ets:insert(tocken_bucket, {111, os:system_time(seconds), 5}),
    ?assertMatch(false, tocken_bucket:is_limit_reached(111,5)),
    ?assertThrow(not_valid_userid, tocken_bucket:is_limit_reached(-123)),
    ?assertThrow(not_valid_userid, tocken_bucket:is_limit_reached(0)).
