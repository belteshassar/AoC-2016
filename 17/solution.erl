% day 17 Erlang solution
-module(solution).
-export([part1/1, part2/1]).


% Return a list of open doors on the form {{DX, DY}, <new Path>}
% from a room given a Key and a Path
doors(Key, Path) ->
    <<U:4, D:4, L:4, R:4, Rest/binary>> = crypto:hash(md5, lists:append(Key, Path)),
    [{{DX, DY}, lists:append(Path, D)} ||
     {C, D, {DX, DY}} <- [{U, "U", {0, -1}}, {D, "D", {0, 1}}, {L, "L", {-1, 0}}, {R, "R", {1, 0}}], C > 10].


% Return true if {X, Y} is within the maze
in_maze({X, Y}) ->
    (0 =< X) and (X =< 3) and (0 =< Y) and (Y =< 3).


% Return a list of the successor states of {{X, Y}, Path}
successors(Key, {{X, Y}, Path}) ->
    [{{X + DX, Y + DY}, Path} || {{DX, DY}, Path} <- doors(Key, Path),
     in_maze({X + DX, Y + DY})].


% Return a list of paths from a starting state or a list of starting states
% to a target state. If no previous path is given, it is assumed to be empty.
search(Key, [], Target) ->
    [];
search(Key, [H|T], Target) ->
    lists:append(
        search(Key, H, Target),
        search(Key, T, Target)
    );
search(Key, {{X, Y}, Path}, Target) ->
    if
        {X, Y} == Target ->
            [{0, Path}];
        true ->
            [{D + 1, Path} || {D, Path} <- search(Key, successors(Key, {{X, Y}, Path}), Target)]
    end;
search(Key, {X, Y}, Target) ->
    search(Key, {{X, Y}, ""}, Target).


% Return solution for part1 (shortest path)
part1(Key) ->
    {D, Path} = lists:min(search(Key, {0, 0}, {3, 3})),
    Path.


% Return solution for part 2 (length of longest path)
part2(Key) ->
    {D, Path} = lists:max(search(Key, {0, 0}, {3, 3})),
    D.
