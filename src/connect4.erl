-module(connect4).

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
  case {Red, Yellow, Full} of
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
  Cols = ListHasFour(connect4_matrix:get_columns(Board)),
  Diags = ListHasFour(connect4_matrix:get_diags(Board)),
  Rows or Cols or Diags.

-spec list_has_four([cell()], color()) -> true | false.
list_has_four(Cells, Color) -> 
  case Cells of
    [Color, Color, Color, Color | _] -> true;
    [_ | T] -> list_has_four(T, Color);
    _ -> false
  end.

-spec get_drop_column(color(), board()) -> integer().
get_drop_column(Player, Board) ->
  Answer = ask_drop_column(Player),
  Analysis = analyze_drop_column_answer(Answer, Board),
  respond_to_drop_column_answer(Analysis),
  case Analysis of
    {ok, Number} -> Number;
    _ -> get_drop_column(Player, Board)
  end.

ask_drop_column(Player) ->
  PlayerName = case Player of
    red -> "Red";
    yellow -> "Yellow"
  end,

  io:format("~s Player, where would you like to drop a piece?~n", [PlayerName]),
  io:fread("Column (1-7): ", "~d").

analyze_drop_column_answer(Result, Board) ->
  case Result of
    {ok, [Number]} when (Number >= 1) and (Number =< 7) -> 
      AreAnyEmpty = lists:member(empty, connect4_matrix:get_column(Number, Board)),
      case AreAnyEmpty of
        true -> {ok, Number};
        false -> column_full
      end;
    _ -> parse_error
  end.

respond_to_drop_column_answer(Analysis) ->
  case Analysis of
    {ok, Number} -> io:format("You chose column ~p.~n", [Number]);
    column_full -> io:format("That column is full.~n");
    parse_error -> io:format("Please enter a number between 1 and 7.~n")
  end.

-spec drop_piece(board(), integer(), color()) -> board().
drop_piece(Board, Column, Color) ->
  Row = find_last_empty_row(Board, Column),
  connect4_list:setnth(Row, Board, connect4_list:setnth(Column, lists:nth(Row, Board), Color)).

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

-spec next_player(color()) -> color().
next_player(red) -> yellow;
next_player(yellow) -> red.
