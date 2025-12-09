module Tests
open Lib
open Lib.Puzzle

open FsUnit
open NUnit.Framework

let sample_input_raw = """
7,1
11,1
11,7
9,7
9,5
2,5
2,3
7,3
"""

let sample_input = sample_input_raw.Trim().Split "\n"

[<TestFixture>]
type ``Parsing Tests`` ()=
    [<Test>]
    member _.``It should parse input`` ()=
        sample_input 
        |> Seq.map Coords.parse 
        |> should equalSeq [
            (7,1)
            (11,1)
            (11,7)
            (9,7)
            (9,5)
            (2,5)
            (2,3)
            (7,3)
        ]

[<TestFixture>]
type ``Coords tests`` ()=
    [<Test>]
    member _.``It should calculate rectangleArea correctly`` ()=
        Coords.rectangleArea (2, 5) (11, 1)
        |> should equal 50

[<TestFixture>]
type ``Tests for solution`` ()=
    [<Test>]
    member _.``It should solve part 1`` ()=
        part1 sample_input
        |> should equal 50

    [<Test>]
    member _.``It should solve part 2`` ()=
        part2 sample_input
        |> should equal "the right answer"
