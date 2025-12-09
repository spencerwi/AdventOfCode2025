module Lib

let allUniquePairs (xs : 'a seq) : ('a * 'a) seq =
    seq {
        let arr = Array.ofSeq xs in 
        for i in 0 .. arr.Length - 2 do
            for j in i+1 .. arr.Length - 1 do
                yield (arr[i], arr[j])
    }

module Coords = begin
    type t = int64 * int64

    let parse (input : string) : t =
        let [|x;y|] = 
            input.Split "," 
            |> Array.map int64
        in
        (x, y)

    let rectangleArea ((ax, ay) : t) ((bx, by) : t) : int64 =
        (abs (by - ay) + 1L) * (abs (bx - ax) + 1L) // we have to add 1 to each because we're _including_ the corners — meaning that the rectangle with corners at (1, 1) and (2, 2) has side lengths of 2 and 2, not 1 and 1
end



module Puzzle = begin
    let part1 (input: string seq) =
        let redTiles = 
            input
            |> Seq.map Coords.parse
        in
        redTiles
        |> allUniquePairs
        |> Seq.map (fun (a, b) -> Coords.rectangleArea a b)
        |> Seq.max

    let part2 (input: string seq) =
        "the right answer"
end
