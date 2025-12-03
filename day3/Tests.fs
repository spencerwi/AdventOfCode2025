module Tests
open Lib
open Puzzle

open FsUnit
open NUnit.Framework

let sample_input_raw = """
987654321111111
811111111111119
234234234234278
818181911112111
"""

let sample_input = sample_input_raw.Trim().Split "\n"

[<TestFixture>]
type ``Parsing Tests`` ()=
    [<Test>]
    member _.``It should parse input`` ()=
        sample_input[0] 
        |> BatteryBank.parse
        |> _.batteries
        |> should equal [|9; 8; 7; 6; 5; 4; 3; 2; 1; 1; 1; 1; 1; 1; 1 |]


let batteryBanks = parseInput sample_input |> Array.ofSeq

[<TestFixture>]
type ``BatteryBank tests`` ()=

    static member joltageTestCases : (BatteryBank * int)[] = [|
        (batteryBanks[0], 98)
        (batteryBanks[1], 89)
        (batteryBanks[2], 78)
        (batteryBanks[3], 92)
    |]

    [<Test>]
    [<TestCaseSource("joltageTestCases")>]
    member _.``It should calculate joltage correctly`` (testData : BatteryBank * int) =
        let batteryBank, expectedResult = testData in
        batteryBank.joltageRating |> should equal expectedResult
        

[<TestFixture>]
type ``Tests for solution`` ()=

    [<Test>]
    member _.``It should solve part 1`` ()=
        part1 batteryBanks
        |> should equal 357

    [<Test>]
    member _.``It should solve part 2`` ()=
        part2 sample_input
        |> should equal "the right answer"
