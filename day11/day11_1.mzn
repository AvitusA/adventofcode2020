include "globals.mzn";

int: ncol;
int: nrow;
int: nfloor;
set of int: neighbors = -1..1;

array[1..nfloor] of 1..nrow: floor_row;
array[1..nfloor] of 1..ncol: floor_col;
array[1..nrow, 1..ncol] of bool: is_floor = array2d(1..nrow, 1..ncol, 
  [exists(k in 1..nfloor)(i = floor_row[k] /\ j = floor_col[k]) | i in 1..nrow, j in 1..ncol]);
array[1..nrow, 1..ncol] of var 0..1: is_occupied;
var int: n_occupied = sum(is_occupied);

function var int: occupied_neighbors(int: row, int: col) =
  sum(i, j in neighbors where 1 <= row + i /\ row + i <= nrow /\ 1 <= col + j /\ col + j <= ncol /\ not (i = 0 /\ j = 0))
      (is_occupied[row + i, col + j]);

constraint forall(i in 1..nfloor)(is_occupied[floor_row[i], floor_col[i]] = 0); % Floor can never be occupied
constraint forall(i in 1..nrow, j in 1..ncol)(
  ((is_occupied[i, j] = 1) -> occupied_neighbors(i, j) < 4) /\
  ((is_occupied[i, j] = 0 /\ not is_floor[i, j]) -> occupied_neighbors(i, j) > 0)
);

solve :: int_search(is_occupied, occurrence, indomain_min)
  maximize n_occupied;

output [format(n_occupied), "\n",
        show2d(array2d(1..nrow, 1..ncol, 
          [if is_floor[i, j] then "." else if is_occupied[i, j] = 0 then "L" else "#" endif endif | i in 1..nrow, j in 1..ncol]))];