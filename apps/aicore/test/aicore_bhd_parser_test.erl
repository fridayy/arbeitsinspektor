-module(aicore_bhd_parser_test).

-include_lib("eunit/include/eunit.hrl").

simple_test() ->
    {ok, Tokens, _} = aicore_bhd_lexer:string("mon-tue[08:00-17:00]"),
    R = aicore_bhd_parser:parse(Tokens),
    ?assertEqual({ok, #{day_range => {1, 2}, from_time => {8, 0, 0}, to_time => {17, 0, 0}}}, R).

ignores_whitespace_test() ->
    {ok, Tokens, _} = aicore_bhd_lexer:string("mon-tue [08:00 - 17:00]"),
    R = aicore_bhd_parser:parse(Tokens),
    ?assertEqual({ok, #{day_range => {1, 2}, from_time => {8, 0, 0}, to_time => {17, 0, 0}}}, R).

can_specify_single_day_test() ->
    {ok, Tokens, _} = aicore_bhd_lexer:string("mon[08:00 - 17:00]"),
    R = aicore_bhd_parser:parse(Tokens),
    ?assertEqual({ok, #{day_single => 1, from_time => {8, 0, 0}, to_time => {17, 0, 0}}}, R).


returns_an_error_on_wrong_time_designation_test() ->
    {ok, Tokens, _} = aicore_bhd_lexer:string("mon[08:00]"),
    R = aicore_bhd_parser:parse(Tokens),
    ?assertMatch({error, _}, R).
