module Tests
open Lib
open Lib.Puzzle

open FsUnit
open NUnit.Framework

let sample_input_raw = """
162,817,812
57,618,57
906,360,560
592,479,940
352,342,300
466,668,158
542,29,236
431,825,988
739,650,466
52,470,668
216,146,977
819,987,18
117,168,530
805,96,715
346,949,466
970,615,88
941,993,340
862,61,35
984,92,344
425,690,689
"""

let sample_input = sample_input_raw.Trim().Split "\n"

[<TestFixture>]
type ``Parsing Tests`` ()=
    [<Test>]
    member _.``It should parse input`` ()=
        sample_input[0] 
        |> JunctionBox.parse
        |> should equal {x = 162; y = 817; z = 812}

[<TestFixture>]
type ``Tests for solution`` ()=
    let (part1, part2) = solve 10 sample_input

    [<Test>]
    member _.``It should solve part 1`` ()=
        part1 
        |> should equal 40

    [<Test>]
    member _.``It should solve part 2`` ()=
        part2 
        |> should equal 25272
