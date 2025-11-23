# Advent of Haskell

Advent of Code solutions written in the best language - Haskell ;)

## Project Setup

I am not using Cabal or Stack because I have the haskell libraries I use managed by my system package manager.

I have written a shell utilty (`aoc.sh`) for managing the compilation and running of each day for me.

### LSP setup

The `hie.yaml` and `.hie-bios.sh` files tell the LSP what compiler options the LSP should use.
The main thing I do here give the path of the `Utils/` dir which has some helper modules and functions I have written.

The `.hie-bios.sh` script will dynamically pass in new modules as they are added to utils.

## Developer Workflow

### Generating a day

Use the following command to generate a new day:

```sh
./aoc.sh new -y 2025 -d 1
```

If you do not pass a day or year argument it will default to the current day or year.
Unless you are in a year or day directory already in which case it will use that as the default.
(The same is true for all other commands e.g. `run`)

The above command will create a directory `./2025/01` with a skeleton `Day.hs` and the input if it was able to be downloaded
(You need to set the AOC\_SESSION env var for this to work).

### Running a day

Use the below command to complie and run any day:

```sh
./aoc.sh run -y 2025 -d 1 parse one two input
```

The bits after the options are what is actually being passed to my Haskell program.
The postional arguments `parse`, `one` and `two` are saying to print the result of parsing and to compute the solution to part one and then two.
The last argument `input` is the input file. You can pass in any file, typically a `sample.txt` such as given in the problem for the day.

So based on the above, I could also run the below. This would only compute the part two solution on the given `sample` input file:

```sh
./aoc run -y 2025 -d 1 two sample
```

### Removing compiled binaries

To delete all compiled binaries, run:

```sh
./aoc clean
```

## TODO

- Add a `submit` operation to the script to submit the answer for a given day

