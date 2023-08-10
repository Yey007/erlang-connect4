-module(connect4).

%% API exports
-export([main/1]).

%%====================================================================
%% API functions
%%====================================================================

-record (game, {board :: connect4_board:board(), player :: connect4_player:color()}).

%% escript Entry point
main(_) ->
    play(#game{board = connect4_board:create_board(), player = red}),
    erlang:halt(0).

%%====================================================================
%% Internal functions
%%====================================================================

-spec play(#game{}) -> ok.
play(#game{board = Board, player = Player}) -> 
  Rendered = connect4_board:render_board(Board),
  io:format("~s\n", [Rendered]),
  Winner = connect4_board:get_winner(Board),
  case Winner of
    red -> io:format("Red Player has won!"), ok;
    yellow -> io:format("Yellow Player has won!"), ok;
    draw -> io:format("It's a draw."), ok;
    noneyet -> 
      Column = get_drop_column(Player, Board),
      play(#game{board = connect4_board:drop_piece(Board, Column, Player), player = connect4_player:next_player(Player)})
  end.

-spec get_drop_column(connect4_player:color(), connect4_board:board()) -> integer().
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
