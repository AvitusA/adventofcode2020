This solution uses both racket, and the awesome constraint modeling language minizinc (https://www.minizinc.org).

Pipe things together like this:

Part 1:
```
$ racket part1_dzn_converter.rkt < input | minizinc part1.mzn -
```

Part 2:
```
$ racket part1_dzn_converter.rkt < input | minizinc part1.mzn - | racket part2.rkt
```
