-module(connect4_board).

-export([create_board/0, get_winner/1, drop_piece/3, render_board/1]).

-type cell() :: empty | connect4_player:color().
-type row() :: [cell()].
-type board() :: [row()].

-spec create_board() -> board().
create_board() -> 
  StartRow = [empty, empty, empty, empty, empty, empty, empty],
  [StartRow, StartRow, StartRow, StartRow, StartRow, StartRow].

-spec get_winner(board()) -> noneyet | draw | connect4_player:color().
get_winner(Board) -> 
  Red = has_four(Board, red),
  Yellow = has_four(Board, yellow),
  Full = is_full(Board),
  case {Red, Yellow, Full} of
    {true, _, _} -> red;
    {_, true, _} -> yellow;
    {false, false, true} -> draw;
    {false, false, false} -> noneyet
  end. 

-spec is_full(board()) -> true | false.
is_full(Board) -> not lists:any(fun(X) -> X == empty end, lists:flatten(Board)).

-spec has_four(board(), connect4_player:color()) -> true | false.
has_four(Board, Color) -> 
  ListHasFour = fun (L) -> lists:any(fun (X) -> list_has_four(X, Color) end, L) end,
  Rows = ListHasFour(Board),
  Cols = ListHasFour(connect4_matrix:get_columns(Board)),
  Diags = ListHasFour(connect4_matrix:get_diags(Board)),
  Rows or Cols or Diags.

-spec list_has_four([cell()], connect4_player:color()) -> true | false.
list_has_four(Cells, Color) -> 
  case Cells of
    [Color, Color, Color, Color | _] -> true;
    [_ | T] -> list_has_four(T, Color);
    _ -> false
  end.

-spec drop_piece(board(), integer(), connect4_player:color()) -> board().
drop_piece(Board, Column, Color) ->
  Row = find_last_empty_row(Board, Column),
  set_cell(Board, Row, Column, Color).

-spec set_cell(board(), integer(), integer(), cell()) -> board().
set_cell(Board, Row, Column, Cell) ->
  connect4_list:setnth(Row, Board, connect4_list:setnth(Column, lists:nth(Row, Board), Cell)).

-spec find_last_empty_row(board(), integer()) -> integer().
find_last_empty_row(Board, Column) ->
  Col = connect4_matrix:get_column(Column, Board),
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