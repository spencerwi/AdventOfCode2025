module Lib
open System.Collections.Generic

type Location = {
    row : int
    col : int
}
    with 
        member this.down() = { this with row = this.row + 1}
        member this.left() = { this with col = this.col - 1}
        member this.right() = { this with col = this.col + 1 }

type Splitter = Location

type Manifold = {
    splitters : Set<Location>
    width : int
    height: int
    startPoint : Location
}
    with
        static member parse (lines : string array) =
            let width = lines[0].Length in
            let height = lines.Length in
            let splitters = Set.ofList [
                for row in 0..height-1 do
                    for col in 0..width-1 do
                        if lines[row][col] = '^' then 
                            yield {row = row; col = col}
            ] in
            let startPoint = {
                row = 0
                col = lines[0].IndexOf 'S'
            } in
            {
                splitters = splitters
                width = width
                height = height
                startPoint = startPoint
            }

        /// <Description>
        /// Runs a single beam path down until it hits a splitter (or the bottom), then yields that splitter location (if it hit one)
        /// </Description>
        member this.runBeamUntilSplitter (beamStart : Location) : Splitter option =
            let mutable seenSplitter = None in
            let mutable currentLocation = beamStart in
            while seenSplitter = None && currentLocation.row < this.height do begin
                if this.splitters.Contains currentLocation then
                    seenSplitter <- Some currentLocation
                else
                    currentLocation <- currentLocation.down()
            end;
            seenSplitter

module Puzzle = begin
    let part1 (input: Manifold) =
        let mutable splittersSeen = Set.empty in
        let beamsToCheck = new Queue<Location>([input.startPoint]) in
        while beamsToCheck.Count > 0 do begin
            let currentBeam = beamsToCheck.Dequeue() in
            match input.runBeamUntilSplitter currentBeam with
            | None -> ()
            | Some nextSplitter when splittersSeen.Contains nextSplitter -> ()
            | Some nextSplitter ->
                splittersSeen <- splittersSeen.Add nextSplitter
                beamsToCheck.Enqueue(nextSplitter.left())
                beamsToCheck.Enqueue(nextSplitter.right())
        end;
        splittersSeen.Count

    let part2 (input: string seq) =
        "the right answer"
end
