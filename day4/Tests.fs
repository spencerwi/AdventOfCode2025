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


    [<Test>]
    member _.``It should solve part 1`` ()=
        sample_input
        |> Grid.parse 
        |> part1 
        |> should equal 13

    [<Test>]
    member _.``It should solve part 2`` ()=
        sample_input
        |> Grid.parse 
        |> part2
        |> should equal 43
