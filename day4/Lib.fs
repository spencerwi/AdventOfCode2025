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
    cells: Tile[,]
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

        member this.Item (location : Location) = 
            this.cells[location.row, location.col]

        member this.Item (row : int, col : int) = 
            this.cells[row, col]

        member this.height = this.cells.GetUpperBound(0) + 1
        member this.width = this.cells.GetUpperBound(1) + 1
        
        member this.isInBounds (location : Location) =
            location.row >= 0 && location.col >= 0 &&
            location.row < this.height && location.col < this.width

        member this.paperLocations = 
            seq {
                for row in 0..this.height-1 do
                    for col in 0..this.width-1 do
                        if this[row, col] = Paper then
                            yield {row = row; col = col}
            }

        member this.neighborsOf (location : Location) =
            seq {
                location.up().left(); location.up(); location.up().right();
                location.left(); location.right();
                location.down().left(); location.down(); location.down().right()
            } 
            |> Seq.filter this.isInBounds
 
module Puzzle = begin
    let part1 (grid: Grid) =
        grid.paperLocations
        |> Seq.filter (fun paperStack -> 
            let neighboringPaperCount = 
                grid.neighborsOf paperStack
                |> Seq.filter (fun l -> grid[l] = Paper)
                |> Seq.length
            in
            neighboringPaperCount < 4
        )
        |> Seq.length

    let part2 (input: string seq) =
        "the right answer"
end
