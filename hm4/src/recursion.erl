-module(recursion).

-export([tail_fac/1, fac/1, duplicate/1, tail_duplicate/1]).

fac(N) when N == 0 -> 1;
fac(N) when N > 0 -> N*fac(N-1).

tail_fac(N) when N >= 0, is_integer(N) ->  tail_fac(N, 1).
tail_fac(0, Acc) -> Acc;
tail_fac(N, Acc) -> tail_fac(N - 1, Acc * N).

duplicate([]) -> [];
duplicate([H|T]) -> [H, H | duplicate(T) ].

tail_duplicate(List) when is_list(List) -> tail_duplicate(List, []).
tail_duplicate([], Acc) -> lists:reverse(Acc);
tail_duplicate([H|T], Acc) -> tail_duplicate(T, [H, H|Acc]).