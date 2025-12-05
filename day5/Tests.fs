module Tests
open Lib
open Lib.Puzzle

open FsUnit
open NUnit.Framework

let sample_input_raw = """
3-5
10-14
16-20
12-18

1
5
8
11
17
32
"""

let sample_input = sample_input_raw.Trim().Split "\n"

[<TestFixture>]
type ``Parsing Tests`` ()=
    [<Test>]
    member _.``It should parse input`` ()=
        PuzzleInput.parse sample_input
        |> should equal {
            freshRanges = seq {
                Range.make 3 5
                Range.make 10 14
                Range.make 16 20
                Range.make 12 18
            }
            ids = seq { 1L; 5L; 8L; 11L; 17L; 32L }
        }

[<TestFixture>]
type ``Tests for Range`` ()=
    static member rangesWithSizes = [|
        (Range.make 3 5, 3L)
        (Range.make 10 14, 5L)
        (Range.make 12 18, 7L)
        (Range.make 16 20, 5L)
    |]

    [<Test>]
    [<TestCaseSource("rangesWithSizes")>]
    member _.``It should report size correctly`` ((range : Range, expectedSize : int64)) =
        range.size |> should equal expectedSize

    static member rangeOverlapTestCases = [|
        (Range.make 3 5, Range.make 10 14, false)
        (Range.make 16 20, Range.make 12 18, true)
        (Range.make 12 18, Range.make 16 20, true)
    |]

    [<Test>]
    [<TestCaseSource("rangeOverlapTestCases")>]
    member _.``It should report overlaps correctly`` ((range1 : Range, range2: Range, expectedResult : bool)) =
        range1.overlapsWith range2 |> should equal expectedResult

    static member fullyEncloseTestCases = [|
        (Range.make 3 5, Range.make 10 14, false)
        (Range.make 10 14, Range.make 3 5, false)
        (Range.make 16 20, Range.make 12 18, false)
        (Range.make 12 18, Range.make 16 20, false)
        (Range.make 10 20, Range.make 16 18, true)
        (Range.make 16 18, Range.make 10 20, false)
        (Range.make 10 20, Range.make 16 20, true)
        (Range.make 16 20, Range.make 10 20, false)
        (Range.make 10 20, Range.make 10 20, true)
    |]

    [<Test>]
    [<TestCaseSource("fullyEncloseTestCases")>]
    member _.``It should report full-enclosure correctly`` ((range1 : Range, range2: Range, expectedResult : bool)) =
        range1.fullyEncloses range2 |> should equal expectedResult

    static member tryMergeWithTestCases = [|
        (Range.make 3 5, Range.make 10 14, None)
        (Range.make 16 20, Range.make 12 18, Some (Range.make 12 20))
        (Range.make 12 18, Range.make 16 20, Some (Range.make 12 20))
        (Range.make 10 20, Range.make 16 18, Some (Range.make 10 20))
        (Range.make 16 18, Range.make 10 20, Some (Range.make 10 20))
        (Range.make 10 20, Range.make 16 20, Some (Range.make 10 20))
        (Range.make 16 20, Range.make 10 20, Some (Range.make 10 20))
        (Range.make 10 20, Range.make 10 20, Some (Range.make 10 20))
        (Range.make 1 5, Range.make 3 7, Some (Range.make 1 7))
        (Range.make 3 7, Range.make 1 5, Some (Range.make 1 7))
        (Range.make 1 3, Range.make 3 7, Some (Range.make 1 7))
        (Range.make 3 7, Range.make 1 3, Some (Range.make 1 7))
        (Range.make 1 3, Range.make 4 7, None)
    |]

    [<Test>]
    [<TestCaseSource("tryMergeWithTestCases")>]
    member _.``It should merge ranges properly`` ((range1 : Range, range2 : Range, exepctedResult : Range option)) =
        range1.tryMergeWith range2 |> should equal exepctedResult
        

[<TestFixture>]
type ``Tests for solution`` ()=

    let puzzleInput = (PuzzleInput.parse sample_input).mergeRanges()

    [<Test>]
    member _.``It should solve part 1`` ()=
        part1 puzzleInput
        |> should equal 3

    [<Test>]
    member _.``It should solve part 2`` ()=
        part2 puzzleInput
        |> should equal 14
