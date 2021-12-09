# Advent of Code 2021 solutions

This repository holds my solutions for [Advent of Code](https://adventofcode.com) 2021.
They are all written entirely in Haskell.

## To use

The included [Makefile](./Makefile) contains targets to build Haskell files as well as run the resulting binaries
using the example inputs and the personal inputs.

(in the following snippets, replace `$DAY` with the number of the day you want to run)

To run one of these, put your input in the file `input/$DAY.txt` and run the following commands:

```sh
make Day$DAY
make run-$DAY
```
This will build the file and run it with the personal inputs.
To use the example input, put it in the file `input/$DAY-test.txt` and run
```sh
make Day$DAY
make test-$DAY
```
