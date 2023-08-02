-module(list_util).

-export([setnth/3]).

-spec setnth(integer(), [A], A) -> [A].
setnth(1, [_ | T], Cell) -> [Cell | T];
setnth(N, [H | T], Cell) -> [H | setnth(N - 1, T, Cell)].
