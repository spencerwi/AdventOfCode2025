module Lib
open System

let maxWithIndex (arr : int64 array) =
    let maxValue = Array.max arr in
    let idx = Array.IndexOf(arr, maxValue) in
    (maxValue, idx)

type BatteryBank = {
    batteries : int64 array
}
    with 
        static member parse (line : string) : BatteryBank =
            {
                batteries = 
                    line
                    |> Seq.map (string >> int64)
                    |> Array.ofSeq
            }

        member this.joltageRating (batteryCount : int) = 
            printfn "Going to turn on %d batteries from among %A" batteryCount this.batteries;
            let mutable batteriesTurnedOnSoFar = 0 in
            let mutable digitsFoundReversed = []; // reversed because of how list cons works
            // We keep a sliding window that "shrinks" from the left whenever we take a digit, so that we only keep the digits *after* that one available for selection
            let mutable availableDigitsWindow = this.batteries;
            while batteriesTurnedOnSoFar < batteryCount do
                // If we're making a 2 digit number, and we're on the first digit, we have to leave at least 1 number available for the second digit, so we put an upper bound on which indexes we can take
                let upperBoundIndexOffset = batteryCount - batteriesTurnedOnSoFar in
                let availableDigits = availableDigitsWindow[0..this.batteries.Length - upperBoundIndexOffset] in
                let highestAvailableDigit, indexOfThatDigit =
                    maxWithIndex availableDigits
                digitsFoundReversed <- highestAvailableDigit :: digitsFoundReversed;
                availableDigitsWindow <- availableDigitsWindow[indexOfThatDigit + 1..];
                batteriesTurnedOnSoFar <- batteriesTurnedOnSoFar + 1;
            
            digitsFoundReversed
            |> List.rev
            |> List.map string
            |> String.concat ""
            |> int64

module Puzzle = begin
    let parseInput (input : string seq) : BatteryBank seq =
        input
        |> Seq.map BatteryBank.parse

    let part1 (input: BatteryBank seq) =
        input
        |> Seq.sumBy (fun bank -> bank.joltageRating 2)

    let part2 (input: BatteryBank seq) =
        input
        |> Seq.sumBy (fun bank -> bank.joltageRating 12)
end
