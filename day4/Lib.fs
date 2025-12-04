module Lib
open System

type Location = {
    row : int
    col : int
}
    with 
        member this.left() : Location = {this with col = this.col - 1}
        member this.right() : Location = {this with col = this.col + 1}
        member this.up() : Location = {this with row = this.row - 1}
        member this.down() : Location = {this with row = this.row + 1}

type Tile =
    | Empty 
    | Paper
    with
        override this.ToString() : string =
            match this with 
            | Empty -> "."
            | Paper -> "@"

        static member parse = function
            | '.' -> Empty
            | '@' -> Paper
            | other -> failwithf "Invalid location char: '%A'" other

type Grid = {
    mutable cells: Tile[,]
}
    with 
        override this.ToString() : string =
            query {
                for row in 0..this.cells.GetUpperBound(0) do
                    yield this.cells[row,*] |> Seq.map _.ToString() |> (String.concat "")
            } |> String.concat "\n"

        static member parse (input: string seq) : Grid =
            {
                cells = 
                    input
                    |> Seq.map (Seq.map Tile.parse)
                    |> array2D
            }

        member this.Item 
            with get (location : Location) = this.cells[location.row, location.col]
            and set (location : Location) (newValue : Tile) = this.cells[location.row, location.col] <- newValue

        member this.height = this.cells.GetUpperBound(0) + 1
        member this.width = this.cells.GetUpperBound(1) + 1
        
        member this.isInBounds (location : Location) =
            location.row >= 0 && location.col >= 0 &&
            location.row < this.height && location.col < this.width

        member this.paperLocations () = 
            seq {
                for row in 0..this.height-1 do
                    for col in 0..this.width-1 do
                        if this.cells[row, col] = Paper then
                            yield {row = row; col = col}
            }

        member this.neighborsOf (location : Location) =
            seq {
                location.up().left(); location.up(); location.up().right();
                location.left(); location.right();
                location.down().left(); location.down(); location.down().right()
            } 
            |> Seq.filter this.isInBounds

        /// Returns the number of papers actually removed 
        member this.removePapersFrom (locations : Location seq) : int =
            let mutable removedCount = 0 in 
            for location in locations do begin
                if this.isInBounds location && this[location] = Paper then
                    removedCount <- removedCount + 1;
                    this[location] <- Empty
            end;
            removedCount

        member this.movablePaperLocations () : Location seq =
            this.paperLocations()
            |> Seq.filter (fun paperStack -> 
                let neighboringPaperCount = 
                    this.neighborsOf paperStack
                    |> Seq.filter (fun l -> this[l] = Paper)
                    |> Seq.length
                in
                neighboringPaperCount < 4
            )

 
module Puzzle = begin
    let part1 (grid: Grid) =
        grid.movablePaperLocations()
        |> Seq.length

    let part2 (grid: Grid) =
        let mutable totalRemoved = 0 in
        let mutable targets = grid.movablePaperLocations() in
        while not (Seq.isEmpty targets) do begin
            totalRemoved <- totalRemoved + grid.removePapersFrom targets;
            targets <- grid.movablePaperLocations()
        end
        totalRemoved
end
