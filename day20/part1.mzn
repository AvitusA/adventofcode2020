include "globals.mzn";

set of int: tile_ids;
int: puzzle_size;
int: tile_size;
int: n_tiles;
int: output_size = puzzle_size * (tile_size - 2);

% tile[i][y][x]
array[1..n_tiles, 1..tile_size, 1..tile_size] of 0..1: tile;
array[1..n_tiles] of tile_ids: tile_id;
% puzzle[y][x]
array[1..puzzle_size, 1..puzzle_size] of var 1..n_tiles: puzzle;

array[1..puzzle_size, 1..puzzle_size] of var tile_ids: tile_in_place;

constraint forall(i in 1..puzzle_size, j in 1..puzzle_size) (tile_in_place[i, j] = tile_id[puzzle[i, j]]);
constraint alldifferent(tile_in_place);

% West edge:
constraint forall(puzzle_x in 2..puzzle_size, puzzle_y in 1..puzzle_size)(
  forall(tile_y in 1..tile_size)(tile[puzzle[puzzle_y, puzzle_x - 1], tile_y, tile_size] = 
                                  tile[puzzle[puzzle_y, puzzle_x], tile_y, 1])
);

% North edge:
constraint forall(puzzle_x in 1..puzzle_size, puzzle_y in 2..puzzle_size)(
  forall(tile_x in 1..tile_size)(tile[puzzle[puzzle_y - 1, puzzle_x], tile_size, tile_x] =
                                  tile[puzzle[puzzle_y, puzzle_x], 1, tile_x]) 
);

solve ::int_search(puzzle, first_fail, indomain_split)
  satisfy; % Use Chuffed or Gecode, should solve in less than 3s
  
output [
show(output_size), "\n",
show(
  array2d(1..output_size, 1..output_size,
    [tile[puzzle[puzzle_y, puzzle_x], y, x] |
      puzzle_y in 1..puzzle_size,
      y in 2..(tile_size - 1),
      puzzle_x in 1..puzzle_size,
      x in 2..(tile_size - 1)])), "\n",
      "corner product (part 1): ",
show(tile_in_place[1, 1] * 
     tile_in_place[1, puzzle_size] * 
     tile_in_place[puzzle_size, 1] * 
     tile_in_place[puzzle_size, puzzle_size]), "\n"];