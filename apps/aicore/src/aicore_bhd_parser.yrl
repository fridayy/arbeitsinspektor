%% arbeitsinspektor business hour LALR-1 grammar
Nonterminals 
T active_time active_day day_range single_day.

Terminals
'-' '[' ']' ':' weekday time time_delimiter.

Rootsymbol T.

T -> active_day '[' active_time '-' active_time ']' : as_map(['$1', {from_time, '$3'}, {to_time, '$5'}]).
active_day -> day_range : '$1'.
active_day -> single_day : {day_single, day_value_of('$1')}.
day_range -> weekday '-' weekday : {day_range, {day_value_of('$1'), day_value_of('$3')}}.
single_day -> weekday : '$1'.
active_time -> time time_delimiter time : {int_value_of('$1'), int_value_of('$3'), 00}.

Erlang code.
value_of(Token) ->
  element(3, Token).

int_value_of(Token) ->
  list_to_integer(value_of(Token)).

day_value_of(Token) ->
  to_day_of_week(value_of(Token)).

as_map(L) ->
  maps:from_list(L).

to_day_of_week("mon") -> 1;
to_day_of_week("tue") -> 2;
to_day_of_week("wed") -> 3;
to_day_of_week("thu") -> 4;
to_day_of_week("fri") -> 5;
to_day_of_week("sat") -> 6;
to_day_of_week("sun") -> 7.
