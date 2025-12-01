module Tests
open Lib
open Lib.Puzzle

open FsUnit
open NUnit.Framework

let sample_input_raw = """
L68
L30
R48
L5
R60
L55
L1
L99
R14
L82
"""

let sample_input = sample_input_raw.Trim().Split "\n"

[<TestFixture>]
type ``Parsing Tests`` ()=
    [<Test>]
    member _.``It should parse input`` ()=
        sample_input
        |> Seq.map Rotation.parse
        |> should equalSeq [
            Rotation.left 68
            Rotation.left 30
            Rotation.right 48
            Rotation.left 5
            Rotation.right 60
            Rotation.left 55
            Rotation.left 1
            Rotation.left 99
            Rotation.right 14
            Rotation.left 82
        ]

[<TestFixture>]
type ``Rotation tests`` ()=
    [<Test>]
    member _.``It should do rotation properly`` ()=
        sample_input
        |> Seq.map Rotation.parse
        |> Seq.scan State.apply State.Initial
        |> should equalSeq [
            State.Initial
            { currentNumber = 82 ; zeroIndicatedCount = 0; zeroPassedCount = 1}
            { currentNumber = 52 ; zeroIndicatedCount = 0; zeroPassedCount = 1}
            { currentNumber = 0 ; zeroIndicatedCount = 1; zeroPassedCount = 1}
            { currentNumber = 95 ; zeroIndicatedCount = 1; zeroPassedCount = 1}
            { currentNumber = 55 ; zeroIndicatedCount = 1; zeroPassedCount = 2}
            { currentNumber = 0 ; zeroIndicatedCount = 2; zeroPassedCount = 2}
            { currentNumber = 99 ; zeroIndicatedCount = 2; zeroPassedCount = 2}
            { currentNumber = 0 ; zeroIndicatedCount = 3; zeroPassedCount = 2}
            { currentNumber = 14 ; zeroIndicatedCount = 3; zeroPassedCount = 2}
            { currentNumber = 32 ; zeroIndicatedCount = 3; zeroPassedCount = 3}
        ]

    [<Test>]
    member _.``It should do rotation left properly more`` ()=
        State.apply State.Initial (Rotation.left 150)
        |> should equal {currentNumber = 0; zeroIndicatedCount = 1; zeroPassedCount = 1}
    [<Test>]
    member _.``It should do rotation right properly more`` ()=
        State.apply State.Initial (Rotation.right 150)
        |> should equal {currentNumber = 0; zeroIndicatedCount = 1; zeroPassedCount = 1}

[<TestFixture>]
type ``Tests for solution`` ()=
    let (part1, part2) = solve sample_input in
    [<Test>]
    member _.``It should solve part 1`` ()=
        part1 |> should equal 3

    [<Test>]
    member _.``It should solve part 2`` ()=
        part2 |> should equal 6 
