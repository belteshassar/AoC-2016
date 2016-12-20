% day 18 Erlang solution
-module(solution).
-export([nsafe/2]).


next_row([E1, E2]) ->
    [];
next_row([E1, E2, E3 | T]) ->
    if
        E1 == E3 ->
            [$. | next_row([E2, E3 | T])];
        true ->
            [$^ | next_row([E2, E3 |T])]
    end.


nsafe(0, First) ->
    0;
nsafe(N, First) ->
    Safe = length([X || X <- First, X == $.]),
    Safe + nsafe(N-1, next_row([$. | lists:append(First, ".")])).
