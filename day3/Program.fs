open System
open Lib 

let read_stdin_lines () : string array =
    Seq.initInfinite (fun _ -> Console.ReadLine())
    |> Seq.takeWhile (fun line -> line <> null)
    |> Array.ofSeq

[<EntryPoint>]
let main args =
    let input = read_stdin_lines() in
    let batteryBanks = Puzzle.parseInput input in
    printfn "Part 1: %A" (Puzzle.part1 batteryBanks);
    printfn "Part 2: %A" (Puzzle.part2 batteryBanks);
    0
