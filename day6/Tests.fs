module Tests
open Lib
open Lib.Puzzle

open FsUnit
open NUnit.Framework

let sample_input_raw = """
123 328  51 64 
 45 64  387 23 
  6 98  215 314
*   +   *   +  
"""

let sample_input = sample_input_raw.Split("\n", System.StringSplitOptions.RemoveEmptyEntries)

[<TestFixture>]
type ``transposeArray2D tests`` ()=
    [<Test>]
    member _.``It should transpose an N x N array2D`` ()=
        array2D [|
            [| 1; 2; 3;|]
            [| 4; 5; 6;|]
            [| 7; 8; 9;|]
        |]
        |> transposeArray2D
        |> should equal (array2D [|
            [| 1; 4; 7;|]
            [| 2; 5; 8;|]
            [| 3; 6; 9;|]
        |])

    [<Test>]
    member _.``It should transpose an M x N array2D`` ()=
        array2D [|
            [| 1; 2; 3;|]
            [| 4; 5; 6;|]
        |]
        |> transposeArray2D
        |> should equal (array2D [|
            [| 1; 4 |]
            [| 2; 5 |]
            [| 3; 6 |]
        |])


[<TestFixture>]
type ``Parsing Tests`` ()=
    [<Test>]
    member _.``It should parse input`` ()=
        Worksheet.parse sample_input
        |> _.cells
        |> should equal (array2D [|
            [|Number 123; Number 45; Number 6; Operation Multiply|]
            [|Number 328; Number 64; Number 98; Operation Add|]
            [|Number 51; Number 387; Number 215; Operation Multiply|]
            [|Number 64; Number 23; Number 314; Operation Add|]
        |])

[<TestFixture>]
type ``Tests for solution`` ()=
    let worksheet = Worksheet.parse sample_input

    [<Test>]
    member _.``It should solve part 1`` ()=
        part1 worksheet
        |> should equal 4277556L

    [<Test>]
    member _.``It should solve part 2`` ()=
        part2 sample_input
        |> should equal 3263827L
