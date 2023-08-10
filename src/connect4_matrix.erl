-module(connect4_matrix).

-export([transpose/1, reflect_y/1, get_column/2, get_columns/1, get_diags/1]).

-type matrix(A) :: [[A]].

-spec transpose(matrix(A)) -> matrix(A).
transpose([[] | _]) -> [];  % only checking if head is empty is enough, all other lists must be empty too
transpose(Matrix) -> [lists:map(fun hd/1, Matrix) | transpose(lists:map(fun tl/1, Matrix))].

-spec reflect_y(matrix(A)) -> matrix(A).
reflect_y(Matrix) -> lists:map(fun lists:reverse/1, Matrix).

-spec get_columns(matrix(A)) -> [[A]].
get_columns(Board) -> transpose(Board).

-spec get_column(integer(), matrix(A)) -> [A].
get_column(Column, Board) -> lists:map(fun (X) -> lists:nth(Column, X) end, Board).

-spec get_diags(matrix(A)) -> [[A]].
get_diags(Board) -> get_forward_diags(Board) ++ get_forward_diags(reflect_y(Board)).

-spec get_forward_diags(matrix(A)) -> [[A]].
get_forward_diags(Board) -> 
  % MxN matrix
  N = length(lists:nth(1, Board)),
  M = length(Board),
  NumForwardDiags = M + N - 1,
  ForwardDiagNumbers = lists:seq(1, NumForwardDiags),
  % Generate pairs (a, b) where a + b = ForwardDiagNumbers + 1
  Pairs = lists:map(fun (K) -> lists:zip(lists:seq(1, K), lists:reverse(lists:seq(1, K))) end, ForwardDiagNumbers),
  Filtered = lists:map(fun (L) -> 
    lists:filter(fun ({A, B}) -> (A =< M) and (B =< N) end, L) 
  end, Pairs),
  % Get actual elements from Board
  lists:map(fun (L) -> 
    lists:map(fun({A, B}) -> lists:nth(B, lists:nth(A, Board)) end, L)
  end, Filtered).