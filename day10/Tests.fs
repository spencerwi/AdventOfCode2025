module Tests
open Lib
open Lib.Puzzle

open FsUnit
open NUnit.Framework

let sample_input_raw = """
[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}
[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}
[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}
"""

let sample_input = sample_input_raw.Trim().Split "\n"

[<TestFixture>]
type ``Parsing Tests`` ()=
    [<Test>]
    member _.``It should parse input`` ()=
        sample_input
        |> Seq.map Machine.parse
        |> should equalSeq [
            { 
                targetLightsState = LightsBank.make [| Off; On; On; Off |]
                buttons = [set [3]; set [1; 3]; set [2]; set [2; 3]; set [0; 2]; set [0; 1]]
                joltageRequirements = [|3; 5; 4; 7|] 
            };
            {
                targetLightsState = LightsBank.make [| Off; Off; Off; On; Off |]
                buttons = [set [0; 2; 3; 4]; set [2; 3]; set [0; 4]; set [0; 1; 2]; set [1; 2; 3; 4]] 
                joltageRequirements = [|7; 5; 12; 7; 2|] 
            };
            { 
                targetLightsState = LightsBank.make [| Off; On; On; On; Off; On |]
                buttons = [set [0; 1; 2; 3; 4]; set [0; 3; 4]; set [0; 1; 2; 4; 5]; set [1; 2]]
                joltageRequirements = [|10; 11; 11; 5; 10; 5|] 
            }
        ]

[<TestFixture>]
type ``Tests for solution`` ()=
    [<Test>]
    member _.``It should solve part 1`` ()=
        part1 sample_input
        |> should equal 7

    [<Test>]
    member _.``It should solve part 2`` ()=
        part2 sample_input
        |> should equal "the right answer"
