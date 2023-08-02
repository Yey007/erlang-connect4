-module(matrix).

-export([transpose/1, reflect_y/1]).

-spec transpose([[A]]) -> [[A]].
transpose([[] | _]) -> [];  % only checking if head is empty is enough, all other lists must be empty too
transpose(Matrix) -> [lists:map(fun hd/1, Matrix) | transpose(lists:map(fun tl/1, Matrix))].

-spec reflect_y([[A]]) -> [[A]].
reflect_y(Matrix) -> lists:map(fun lists:reverse/1, Matrix).