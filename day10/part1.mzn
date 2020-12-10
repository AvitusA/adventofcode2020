include "globals.mzn";
set of int: available_adapters;
int: n_adapters = length(available_adapters) + 1; % include the built in adapter
int: device_joltage = max(available_adapters) + 3;
array[0..n_adapters] of var (available_adapters union {0, device_joltage}): adapter;

constraint alldifferent(adapter);
constraint adapter[0] = 0;
constraint forall(i in 1..n_adapters) (adapter[i] - adapter[i-1] in {0, 1, 2, 3});

solve satisfy;
output [format(sum(i in 1..n_adapters)(bool2int(adapter[i] - adapter[i-1] = 1)) 
               * (sum(i in 1..n_adapters)(bool2int(adapter[i] - adapter[i-1] = 3))))];