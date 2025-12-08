module Tests
open Lib
open Lib.Puzzle

open FsUnit
open NUnit.Framework

let sample_input_raw = """
.......S.......
...............
.......^.......
...............
......^.^......
...............
.....^.^.^.....
...............
....^.^...^....
...............
...^.^...^.^...
...............
..^...^.....^..
...............
.^.^.^.^.^...^.
...............
"""

let sample_input = sample_input_raw.Trim().Split "\n"

[<TestFixture>]
type ``Parsing Tests`` ()=
    [<Test>]
    member _.``It should parse input`` ()=
        1 |> should equal 1 // TODO

[<TestFixture>]
type ``Tests for solution`` ()=

    let manifold = Manifold.parse sample_input

    [<Test>]
    member _.``It should solve part 1`` ()=
        part1 manifold
        |> should equal 21

    [<Test>]
    member _.``It should solve part 2`` ()=
        part2 sample_input
        |> should equal "the right answer"
