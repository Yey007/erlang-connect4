-module(connect4_player).

-export([next_player/1]).

-type color() :: red | yellow.

-spec next_player(color()) -> color().
next_player(red) -> yellow;
next_player(yellow) -> red.