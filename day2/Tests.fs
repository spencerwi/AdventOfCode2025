module Tests
open Lib
open Lib.Puzzle

open FsUnit
open NUnit.Framework

let sample_input = "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124"


[<TestFixture>]
type ``Parsing Tests`` ()=
    [<Test>]
    member _.``Ranges should parse correctly`` ()=
        Range.parse "11-22" 
        |> should equal {startInclusive = 11; stopInclusive = 22}

[<TestFixture>]
type ``Range tests`` ()=
    [<Test>]
    member _.``It should detect invalid ids in small range`` ()=
        let mutable results = RangeCheckResults.Initial in
        let range = Range.parse "95-115"
        range.detectInvalid results;
        results.singleRepeats 
        |> should equalSeq (Set.ofList [99L])
        results.atLeastOnceRepeats
        |> should equalSeq (Set.ofList [99L; 111L])
        results.checkedIds
        |> should equalSeq (Set.ofList [95L..115L])
        

[<TestFixture>]
type ``Tests for solution`` ()=
    let ranges = (parseInput sample_input)
    let part1, part2 = solve ranges

    [<Test>]
    member _.``It should solve part 1`` ()=
        part1
        |> should equal 1227775554L

    [<Test>]
    member _.``It should solve part 2`` ()=
        part2 
        |> should equal 4174379265L
