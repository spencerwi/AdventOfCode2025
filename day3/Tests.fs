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

    static member joltage2TestCases : (BatteryBank * int64)[] = [|
        (batteryBanks[0], 98L)
        (batteryBanks[1], 89L)
        (batteryBanks[2], 78L)
        (batteryBanks[3], 92L)
    |]
    static member joltage3TestCases : (BatteryBank * int64)[] = [|
        (batteryBanks[0], 987L)
        (batteryBanks[1], 819L)
        (batteryBanks[2], 478L)
        (batteryBanks[3], 921L)
    |]
    static member joltage12TestCases : (BatteryBank * int64)[] = [|
        (batteryBanks[0], 987654321111L)
        (batteryBanks[1], 811111111119L)
        (batteryBanks[2], 434234234278L)
        (batteryBanks[3], 888911112111L)
    |]

    [<Test>]
    [<TestCaseSource("joltage2TestCases")>]
    member _.``It should calculate joltage correctly for 2 batteries`` (testData : BatteryBank * int64) =
        let batteryBank, expectedResult = testData in
        batteryBank.joltageRating 2 |> should equal expectedResult
        
    [<Test>]
    [<TestCaseSource("joltage3TestCases")>]
    member _.``It should calculate joltage correctly for 3 batteries`` (testData : BatteryBank * int64) =
        let batteryBank, expectedResult = testData in
        batteryBank.joltageRating 3 |> should equal expectedResult

    [<Test>]
    [<TestCaseSource("joltage12TestCases")>]
    member _.``It should calculate joltage correctly for 12 batteries`` (testData : BatteryBank * int64) =
        let batteryBank, expectedResult = testData in
        batteryBank.joltageRating 12 |> should equal expectedResult

[<TestFixture>]
type ``Tests for solution`` ()=

    [<Test>]
    member _.``It should solve part 1`` ()=
        part1 batteryBanks
        |> should equal 357

    [<Test>]
    member _.``It should solve part 2`` ()=
        part2 batteryBanks
        |> should equal 3121910778619L
