module Tests
open Lib
open Lib.Puzzle

open FsUnit
open NUnit.Framework

let sample_input_raw = """
aaa: you hhh
you: bbb ccc
bbb: ddd eee
ccc: ddd eee fff
ddd: ggg
eee: out
fff: out
ggg: out
hhh: ccc fff iii
iii: out
"""

let sample_input = sample_input_raw.Trim().Split "\n"

[<TestFixture>]
type ``Parsing Tests`` ()=
    [<Test>]
    member _.``It should parse input`` ()=
        sample_input
        |> ServerRack.parse 
        |> _.deviceConnections
        |> should equal (Map.ofSeq [
            ("aaa", set ["you"; "hhh"])
            ("you", set ["bbb"; "ccc"])
            ("bbb", set ["ddd"; "eee"])
            ("ccc", set ["ddd"; "eee"; "fff"])
            ("ddd", set ["ggg"])
            ("eee", set ["out"])
            ("fff", set ["out"])
            ("ggg", set ["out"])
            ("hhh", set ["ccc"; "fff"; "iii"])
            ("iii", set ["out"])
        ])

[<TestFixture>]
type ``Tests for solution`` ()=

    [<Test>]
    member _.``It should solve part 1`` ()=
        sample_input
        |> ServerRack.parse 
        |> part1 
        |> should equal 5

    [<Test>]
    member _.``It should solve part 2`` ()=
        let part2RawInput = """
svr: aaa bbb
aaa: fft
fft: ccc
bbb: tty
tty: ccc
ccc: ddd eee
ddd: hub
hub: fff
eee: dac
dac: fff
fff: ggg hhh
ggg: out
hhh: out
        """ in
        let part2Input = part2RawInput.Trim().Split("\n", System.StringSplitOptions.TrimEntries ||| System.StringSplitOptions.RemoveEmptyEntries)
        part2Input
        |> ServerRack.parse
        |> part2
        |> should equal 2
