module Tests
open System
open Lib
open Puzzle

open FsUnit
open NUnit.Framework

let sample_input_raw = """
..@@.@@@@.
@@@.@.@.@@
@@@@@.@.@@
@.@@@@..@.
@@.@@@@.@@
.@@@@@@@.@
.@.@.@.@@@
@.@@@.@@@@
.@@@@@@@@.
@.@.@@@.@.
"""

let sample_input = sample_input_raw.Trim().Split "\n"

[<TestFixture>]
type ``Parsing Tests`` ()=
    [<Test>]
    member _.``It should parse input`` ()=
        let small_grid_raw = """
        @.@
        .@.
        ...
        """ in
        let parsedGrid = 
            small_grid_raw.Trim().Split("\n", StringSplitOptions.RemoveEmptyEntries ||| StringSplitOptions.TrimEntries)
            |> Grid.parse
        in
        parsedGrid 
        |> should equal {cells = array2D [|
            [|Paper; Tile.Empty; Paper|]
            [|Tile.Empty; Paper; Tile.Empty|]
            [|Tile.Empty; Tile.Empty; Tile.Empty|]
        |]}

[<TestFixture>]
type ``Tests for solution`` ()=

    let grid = Grid.parse sample_input

    [<Test>]
    member _.``It should solve part 1`` ()=
        part1 grid
        |> should equal 13

    [<Test>]
    member _.``It should solve part 2`` ()=
        part2 sample_input
        |> should equal "the right answer"
