% day 19 Erlang solution
-module(solution).
-export([part1/1, part2/1]).

part1(1) ->
    1;
part1(N) ->
    % Once we've removed one elf, N-1 elves remain, just
    % shift labels by two, wrapping around from N to 1
    (part1(N-1) + 1) rem N + 1.


part2(1) ->
    1;
part2(N) ->
    % Once we've removed one elf, N-1 elves remain, just
    % shift labels by one or two, wrapping around from N to 1,
    % depending on whether the winner is before or after the
    % removed elf.
    PrevWinner = part2(N-1),
    if
        2*PrevWinner > N ->
            (PrevWinner + 1) rem N + 1;
        true ->
            PrevWinner + 1
    end.
