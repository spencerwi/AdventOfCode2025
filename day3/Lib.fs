module Lib
open System

let maxWithIndex (arr : int array) =
    let maxValue = Array.max arr in
    let idx = Array.IndexOf(arr, maxValue) in
    (maxValue, idx)

type BatteryBank = {
    batteries : int array
}
    with 
        static member parse (line : string) : BatteryBank =
            {
                batteries = 
                    line
                    |> Seq.map (string >> int)
                    |> Array.ofSeq
            }

        member this.joltageRating = 
            let highestFirstDigit, indexOfFirstDigit = 
                this.batteries[0..this.batteries.Length - 2]
                |> maxWithIndex
            in
            let highestSecondDigit = 
                this.batteries[indexOfFirstDigit + 1..]
                |> Array.max
            in
            (highestFirstDigit * 10) + highestSecondDigit

module Puzzle = begin
    let parseInput (input : string seq) : BatteryBank seq =
        input
        |> Seq.map BatteryBank.parse

    let part1 (input: BatteryBank seq) =
        input
        |> Seq.sumBy _.joltageRating

    let part2 (input: string seq) =
        "the right answer"
end
