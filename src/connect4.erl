-module(connect4).

-import(list_util, [setnth/3]).
-import(matrix, [transpose/1, reflect_y/1]).

%% API exports
-export([main/1]).

%%====================================================================
%% API functions
%%====================================================================

-type color() :: red | yellow.
-type cell() :: empty | color().
-type row() :: [cell()].
-type board() :: [row()].
-record (game, {board :: board(), player :: color()}).

%% escript Entry point
main(_) ->
    StartRow = lists:duplicate(7, empty),
    StartBoard = lists:duplicate(6, StartRow),
    play(#game{board = StartBoard, player = red}),
    erlang:halt(0).

%%====================================================================
%% Internal functions
%%====================================================================

-spec play(#game{}) -> ok.
play(#game{board = Board, player = Player}) -> 
  Rendered = render_board(Board),
  io:format("~s\n", [Rendered]),
  Winner = get_winner(Board),
  case Winner of
    red -> io:format("Red Player has won!"), ok;
    yellow -> io:format("Yellow Player has won!"), ok;
    draw -> io:format("It's a draw."), ok;
    noneyet -> 
      Column = get_drop_column(Player, Board),
      play(#game{board = drop_piece(Board, Column, Player), player = next_player(Player)})
  end.

-spec get_winner(board()) -> noneyet | draw | color().
get_winner(Board) -> 
  Red = has_four(Board, red),
  Yellow = has_four(Board, yellow),
  Full = is_full(Board),
  Cases = {Red, Yellow, Full},
  case Cases of
    {true, _, _} -> red;
    {_, true, _} -> yellow;
    {false, false, true} -> draw;
    {false, false, false} -> noneyet
  end. 

-spec is_full(board()) -> true | false.
is_full(Board) -> not lists:any(fun(X) -> X == empty end, lists:flatten(Board)).

-spec has_four(board(), color()) -> true | false.
has_four(Board, Color) -> 
  ListHasFour = fun (L) -> lists:any(fun (X) -> list_has_four(X, Color) end, L) end,
  Rows = ListHasFour(Board),
  Cols = ListHasFour(get_columns(Board)),
  Diags = ListHasFour(get_diags(Board)),
  Rows or Cols or Diags.

-spec list_has_four([cell()], color()) -> true | false.
list_has_four(Cells, Color) -> 
  case Cells of
    [Color, Color, Color, Color | _] -> true;
    [_ | T] -> list_has_four(T, Color);
    _ -> false
  end.

-spec get_columns(board()) -> [cell()].
get_columns(Board) -> transpose(Board).

-spec get_column(integer(), board()) -> [cell()].
get_column(Column, Board) -> lists:map(fun (X) -> lists:nth(Column, X) end, Board).

-spec get_diags(board()) -> [cell()].
get_diags(Board) -> get_forward_diags(Board) ++ get_forward_diags(reflect_y(Board)).

-spec get_forward_diags(board()) -> [cell()].
get_forward_diags(Board) -> 
  % MxN matrix
  N = length(lists:nth(1, Board)),
  M = length(Board),
  NumForwardDiags = M + N - 1,
  ForwardDiagNumbers = lists:seq(1, NumForwardDiags),
  % Generate pairs (a, b) where a + b = ForwardDiagNumbers + 1
  Pairs = lists:map(fun (K) -> lists:zip(lists:seq(1, K), lists:reverse(lists:seq(1, K))) end, ForwardDiagNumbers),
  % Filter pairs such that a <= M and b <= N
  Filtered = lists:map(fun (L) -> 
    lists:filter(fun ({A, B}) -> (A =< M) and (B =< N) end, L) 
  end, Pairs),
  % Get actual elements from Board
  lists:map(fun (L) -> 
    lists:map(fun({A, B}) -> lists:nth(B, lists:nth(A, Board)) end, L)
  end, Filtered).

-spec get_drop_column(color(), board()) -> integer().
get_drop_column(Player, Board) ->
  PlayerName = case Player of
    red -> "Red";
    yellow -> "Yellow"
  end,

  io:format("~s Player, where would you like to drop a piece?~n", [PlayerName]),
  Result = io:fread("Column (1-7): ", "~d"),

  case Result of
    {ok, [Number]} when (Number >= 1) and (Number =< 7) -> 
      IsEmpty = lists:any(fun(X) -> X == empty end, get_column(Number, Board)),
      case IsEmpty of
        true -> Number;
        false -> io:format("Invalid column.~n"), get_drop_column(Player, Board)
      end;
    _ -> io:format("Invalid column.~n"), get_drop_column(Player, Board)
  end.

-spec drop_piece(board(), integer(), color()) -> board().
drop_piece(Board, Column, Color) ->
  Row = find_last_empty_row(Board, Column),
  setnth(Row, Board, setnth(Column, lists:nth(Row, Board), Color)).

-spec find_last_empty_row(board(), integer()) -> integer().
find_last_empty_row(Board, Column) ->
  Col = get_column(Column, Board),
  WithIndex = lists:zip(Col, lists:seq(1, length(Col))),
  OnlyEmpty = lists:filter(fun ({Cell, _}) -> Cell == empty end, WithIndex),
  {_, Index} = lists:last(OnlyEmpty),
  Index.

-spec render_board(board()) -> string().
render_board(Board) -> lists:join("\n", lists:map(fun render_row/1, Board)).

-spec render_row(row()) -> string().
render_row(Row) -> lists:join(" ", lists:map(fun render_cell/1, Row)).

-spec render_cell(cell()) -> string().
render_cell(empty) -> "o";
render_cell(red) -> "r";
render_cell(yellow) -> "y".

-spec next_player(color()) -> color().
next_player(red) -> yellow;
next_player(yellow) -> red.
