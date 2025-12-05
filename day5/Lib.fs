module Lib
open System
open System.Text.RegularExpressions

type Id = int64
let parseId = int64

type Range = {
    startInclusive : Id
    stopInclusive : Id
}
    with 
        static member PARSING_PATTERN = new Regex("(?<start>\d+)-(?<stop>\d+)")
        static member parse (input : string) =
            let rmatch = Range.PARSING_PATTERN.Match input  in
            if not rmatch.Success then
                failwithf "Invalid range specifier: %s" input
            else
                (parseId (rmatch.Groups["start"].Value), parseId (rmatch.Groups["stop"].Value))
                ||> Range.make
            
        static member make start stop = { startInclusive = start; stopInclusive = stop }
            
        member this.includes (id : Id) = 
            id >= this.startInclusive && id <= this.stopInclusive

        member this.size =
            (this.stopInclusive - this.startInclusive) + 1L // add one because we're inclusive of both stop and start; so a range from 1 to 3 has 3 numbers in it, even though 3 - 1 = 2

        member this.overlapsWith (other : Range) =
            this.includes other.startInclusive || 
            this.includes other.stopInclusive || 
            other.includes this.startInclusive ||
            other.includes this.stopInclusive

        member this.fullyEncloses (other : Range) =
            other.startInclusive >= this.startInclusive &&
            other.stopInclusive <= this.stopInclusive

        member this.tryMergeWith (other : Range) =
            if not (this.overlapsWith other) then
                None
            else
                // Overlap cases to consider:
                //  1. other fully encloses this
                //  2. this fully encloses other
                //  3. other extends leftward of this
                //  4. this extends leftward of other
                //  5. other extends rightward of this
                //  6. this extends rightward of other
                if this.fullyEncloses other then
                    Some this
                elif other.fullyEncloses this then
                    Some other
                else 
                    // We know by now that we overlap, and if we're not fully enclosing, then we're extending either leftward or rightward. 
                    // That just means we should take the smaller start and the larger end; it turns out that works out either way.
                    Some { 
                        startInclusive = (min this.startInclusive other.startInclusive)
                        stopInclusive = (max this.stopInclusive other.stopInclusive)
                    }

let mergeRanges (ranges : Range seq) : Range seq =
    ranges
    |> Seq.sortBy _.startInclusive
    |> Seq.fold (fun (acc : Range list) current ->
        match acc with
        | [] -> [current]
        | prev :: rest ->
            match prev.tryMergeWith current with
            | Some merged -> merged :: rest
            | None -> current :: acc
    ) []
    |> Seq.rev

type MergedRangesPuzzleInput(ranges : Range seq, ids : Id seq) =
    let _mergedRanges = mergeRanges ranges
    let _ids = ids
    with
        member _.ids with get() = _ids
        member _.ranges with get() = _mergedRanges


type PuzzleInput = {
    freshRanges : Range seq
    ids : Id seq
}
    with 
        static member parse (input : string seq) : PuzzleInput =
            let ranges = 
                input
                |> Seq.takeWhile (not << String.IsNullOrWhiteSpace)
                |> Seq.map Range.parse
            in
            let ids =
                input 
                |> Seq.skipWhile (fun line -> line.Contains "-" || String.IsNullOrWhiteSpace line)
                |> Seq.map parseId
            in
            { freshRanges = ranges; ids = ids }

        member this.mergeRanges() : MergedRangesPuzzleInput =
            new MergedRangesPuzzleInput(this.freshRanges, this.ids)

        

module Puzzle = begin
    let part1 (puzzleInput : MergedRangesPuzzleInput) =
        puzzleInput.ids
        |> Seq.filter (fun id -> 
            puzzleInput.ranges
            |> Seq.exists (fun range -> range.includes id)
        )
        |> Seq.length

    let part2 (puzzleInput : MergedRangesPuzzleInput) =
        puzzleInput.ranges
        |> Seq.sumBy _.size
end
