#!/usr/bin/env bash

set -e  # Exit script on error

# Defaults
YEAR=`date +%Y`
DAY=`date +%d`
INPUT="String"
OUTPUT="Int"
DRY_RUN=0
AOC_BASE_URL="https://adventofcode.com"
DOWNLOAD=true

# Change Directory to script root and update default year / day along the way
SCRIPT_ROOT_DIRNAME="advent-of-code"
CURRENT_DIRNAME=`basename $(pwd)`
PARENT_DIRNAME=`basename $(dirname $(pwd))`
if [ "$CURRENT_DIRNAME" != "$SCRIPT_ROOT_DIRNAME" ]; then
  if [ "$PARENT_DIRNAME" == "$SCRIPT_ROOT_DIRNAME" ]; then
    # We are in a year directory
    YEAR=$CURRENT_DIRNAME
    cd ..
  else
    # We are in a day directory and the parent directory is the year
    YEAR=$PARENT_DIRNAME
    DAY=$CURRENT_DIRNAME
    cd ../..
  fi
fi
if [ `basename $(pwd)` != "$SCRIPT_ROOT_DIRNAME" ]; then
  echo "Error: script can only be run from a $SCRIPT_ROOT_DIRNAME directory!"
  exit 1
fi

showHelp() {
cat << EOF  
Usage: ./aoc <OPERATION> <OPTIONS>
Utility for creating and running advent of code solutions

OPERATION:
  new                         Generate a new day, creating the directory at <YEAR>/<DAY>/ with a Day.hs and the downloaded user input (must have AOC_SESSION set)
    EXAMPLE:
      ./aoc.sh new -y 2025 -d 1 -i [(Int,Int)] -o String --dry-run
  
  run <ARGS> <FILE>           Compile and run the Haskell executable for the day's solutions. Pass one of the below <ARGS> to the executable and the <FILE> to use as input
    ARGS:
      parse                   Outputs the result of parsing the day's input
      one                     Compute and print the solution to part one
      two                     Compute and print the solution to part two
    EXAMPLE:
      ./aoc.sh run -y 2025 -d 1 parse one two input

  clean                       Delete all compiled *.hi, *.o and Day binaries

  format                      Format all Haskell source files

OPTIONS:
  -h, --help                  Display this help

  -y, --year                  The year in which to create the given day (current default: $YEAR)

  -d, --day                   The day to create in the given year (current default: $DAY)

  -i, --input                 When creating a new day - The Haskell input type of the solutions after parsing e.g. Int or [[String]] (defaults to String)

  -o, --output                When creating a new day - The Haskell output type of the solutions e.g. Int or String (defaults to Int)

  --dry-run                   When creating a new day - Simulate the day creation, but don't create any dirs/files

  --no-curl                   When creating a new day - Don't attempt to download the input from $AOC_BASE_URL
EOF
}

# Parse options
OPTIONS=$(getopt -l "help,year:,day:,input:,output:,dry-run,no-curl" -o "hy:d:i:o:" -- "$@")
eval set -- "$OPTIONS"
while true; do
  case "$1" in
    -h|--help) showHelp; exit 0;;
    -y|--year) shift; YEAR="$1";;
    -d|--day) shift; DAY="$1";;
    -i|--input) shift; INPUT="$1";;
    -o|--output) shift; OUTPUT="$1";;
    --dry-run) DRY_RUN=1;;
    --no-curl) DOWNLOAD=0;;
    --) shift; break;;
  esac
  shift
done

# Convert day / year to int
YEAR=`echo $YEAR | bc`
DAY=`echo $DAY | bc`
DAY_2D=`printf %02d $DAY`
DAY_DIR="$YEAR/$DAY_2D/"

# Function to generate the day and year dir if it doesn't exist, and create 
# a Day.hs along with an input downloaded from adventofcode.com
generateDay(){
  CONTENT=`cat <<EOF
module Day where

import Advent (dayRunner)

-- Types
type Input = $INPUT
type Output = $OUTPUT

-- Parsing
parseInput :: String -> Input
parseInput = error "Not Implemented"

-- Solutions
partOne :: Input -> Output
partOne = error "Not Implemented"

partTwo :: Input -> Output
partTwo = error "Not Implemented"

-- Main
main :: IO [()]
main = dayRunner parseInput partOne partTwo

EOF
  `

  if [ $DRY_RUN -eq 1 ]; then
      echo "Year: $YEAR"
      echo "Day: $DAY"
      echo "Template: "
      echo "$CONTENT"
      exit
  fi

  mkdir -p $DAY_DIR
  cd $DAY_DIR

  if [ ! -f Day.hs ]; then
    echo "$CONTENT" > Day.hs
  else
    echo "Skipping already existing Day.hs file..."
  fi

  if [ ! -f README.md ]; then
    echo "## Day $DAY_2D" > README.md
  else
    echo "Skipping already existing README.md file..."
  fi

  if [ ! -f input ]; then
    if [ "$DOWNLOAD" == true ] && [ -n "$AOC_SESSION" ]; then
      echo "Downloading input..."
      curl --cookie "session=$AOC_SESSION" "$AOC_BASE_URL/$YEAR/day/$DAY/input" > input
    else
      echo "Skipping download of input from $AOC_BASE_URL. Is your \$AOC_SESSION env var set?"
    fi
  else
    echo "Skipping already existing input file..."
  fi

  echo "Created new day at: ./$DAY_DIR"
  ls
}

runDay() {
  if [ ! -d "$DAY_DIR" ]; then
    echo "Error: $DAY_DIR doesn't exist yet!"
    exit 1
  fi
  cd "$DAY_DIR"

  echo "Running $YEAR Day $DAY_2D..."

  # -keep-hi-files -no-keep-o-files
  ghc -dynamic -i ../../Utils/*.hs -main-is Day Day.hs

  ./Day $@
}

deleteCompiled() {
  echo "Deleting..."
  find . -regextype posix-extended -regex '.*\.(hi|o)$' -print -delete
  find . -regextype posix-extended -regex '.*/[0-9]{4}/[0-9]{2}/Day$' -print -delete
}

formatHaskell() {
  stylish-haskell -i **/*.hs
}

# Check first positional argument for operation to do
case $1 in
  new) shift; generateDay $@;;
  run) shift; runDay $@;;
  clean) deleteCompiled $@;;
  format) formatHaskell $@;;
  *) showHelp; exit 1;;
esac

