module Lib
open System
open System.Text.RegularExpressions

module IdChecks = begin

    type Result = 
        | Valid
        | InvalidSingleRepeat
        | InvalidMultipleRepeat
    
    let REPEAT_ONCE_PATTERN = new Regex("^(\d+)\1$")
    let REPEAT_AT_LEAST_ONCE_PATTERN = new Regex("^(\d+)\1+$")

    let check (number : int64) : Result =
        let stringified = string number in
        if REPEAT_ONCE_PATTERN.IsMatch stringified then
            InvalidSingleRepeat
        elif REPEAT_AT_LEAST_ONCE_PATTERN.IsMatch stringified then
            InvalidMultipleRepeat
        else
            Valid

end

type RangeCheckResults = {
    mutable singleRepeats : Set<int64>
    mutable atLeastOnceRepeats : Set<int64>
    mutable checkedIds : Set<int64>
} 
    with
        static member Initial = 
            { 
                singleRepeats = Set.empty
                atLeastOnceRepeats = Set.empty
                checkedIds = Set.empty
            }

        member this.update (id : int64) (checkResult : IdChecks.Result) =
            this.checkedIds <- this.checkedIds.Add id;
            match checkResult with
                | IdChecks.Result.InvalidSingleRepeat -> 
                    this.singleRepeats <- this.singleRepeats.Add id;
                    this.atLeastOnceRepeats <- this.atLeastOnceRepeats.Add id
                | IdChecks.Result.InvalidMultipleRepeat ->
                    this.atLeastOnceRepeats <- this.atLeastOnceRepeats.Add id
                | _ -> ()

        member this.addInvalid (id : int64) = 
            this.singleRepeats <- Set.add id this.singleRepeats

        member this.addChecked (id : int64) = 
            this.checkedIds <- Set.add id this.checkedIds

        member this.hasAlreadyChecked (id : int64) =
            this.checkedIds.Contains id

type Range = {
    startInclusive : int64
    stopInclusive : int64
}
    with 
        static member make (start, stop) : Range = { startInclusive = start; stopInclusive = stop }
        static member parse (input : string) : Range =
            let pattern = new Regex("(?<start>\d+)-(?<stop>\d+)") in
            let regexMatch = pattern.Match(input) in
            if not (regexMatch.Success) then
                failwithf "invalid range: %s" input
            else
                {
                    startInclusive = int64 (regexMatch.Groups["start"].Value)
                    stopInclusive = int64 (regexMatch.Groups["stop"].Value)
                }

        member this.detectInvalid (previousResults : RangeCheckResults) = 
            for id in this.startInclusive .. this.stopInclusive do
                if not (previousResults.hasAlreadyChecked id) then
                    previousResults.update id (IdChecks.check id)

module Puzzle = begin

    let parseInput (input : string) : Range seq =
        input.Split(",", StringSplitOptions.TrimEntries ||| StringSplitOptions.RemoveEmptyEntries) 
        |> Seq.map Range.parse

    let solve (input : Range seq) : (int64 * int64) =
        let mutable results = RangeCheckResults.Initial in
        for range in input do
            range.detectInvalid results

        (Seq.sum results.singleRepeats, Seq.sum results.atLeastOnceRepeats)

end
