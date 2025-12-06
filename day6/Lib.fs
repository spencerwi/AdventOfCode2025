module Lib
open System

let transposeArray2D (arr : 'a[,]) =
    let rows = Array2D.length1 arr in
    let cols = Array2D.length2 arr in 
    Array2D.init cols rows (fun row col -> arr[col, row])

type MathOp = 
    | Add 
    | Multiply
    with
        member this.fn =
            match this with
            | Add -> (+)
            | Multiply -> (*)

        member this.identityValue =
            match this with
            | Add -> 0
            | Multiply -> 1

type Cell =
    | Number of int64
    | Operation of MathOp
    with 
        static member parse = function
            | "+" -> Operation Add
            | "*" -> Operation Multiply
            | other -> 
                let (_, number) = Int64.TryParse other in
                Number number

type Problem = {
    numbers : int64 seq
    operation : MathOp
}
    with 
        member this.evaluate() : int64 =
            this.numbers
            |> Seq.fold this.operation.fn this.operation.identityValue
            
type Worksheet = {
    cells : Cell[,]
}
    with 
        static member parse (input : string seq) =
            let cells = 
                array2D [|
                    for line in input do 
                        yield [|
                            for cellStr in line.Split(" ", StringSplitOptions.RemoveEmptyEntries) do
                                yield Cell.parse cellStr
                        |]
                |] |> transposeArray2D
            in {cells = cells}

        member this.problems : Problem seq =
            seq {
                for row in 0 .. this.cells.GetUpperBound(0) do
                    let (Operation operation) = this.cells[row, this.cells.GetUpperBound(1)] in
                    let numbers = 
                        this.cells[row, 0..this.cells.GetUpperBound(1) - 1]
                        |> Seq.map (fun (Number n) -> n)
                    in
                    { numbers = numbers; operation = operation }
            }


module Puzzle = begin
    let part1 (worksheet: Worksheet) =
        worksheet.problems
        |> Seq.sumBy _.evaluate()

    let part2 (input: string seq) =
        "the right answer"
end
