module Lib
open System
open System.Text.RegularExpressions

let transposeArray2D (arr : 'a[,]) =
    let rows = Array2D.length1 arr in
    let cols = Array2D.length2 arr in 
    Array2D.init cols rows (fun row col -> arr[col, row])

let splitOnBlankLine (input : string seq) : string list list =
    input 
    |> Seq.fold (fun (groups, current) line -> 
        if String.IsNullOrWhiteSpace line then
            ((List.rev current) :: groups, [])
        else
            (groups, line :: current)
    ) ([], [])
    |> fun (groups, current) -> 
        if List.isEmpty current then groups
        else (List.rev current) :: groups
    |> List.rev

type MathOp = 
    | Add 
    | Multiply
    with
        static member parse = function
            | "*" -> Multiply
            | "+" -> Add
            | other -> failwithf "invalid MathOp: %s" other

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
        static member parse (input : string array) =
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

let parseCephalopod (input : string array) : Problem seq =
    let transposedCharArray2d : char[,] = 
        input
        |> Seq.map (Array.ofSeq)
        |> Seq.map (Array.rev)
        |> array2D
        |> transposeArray2D

    let restringified = seq {
        for row in 0..transposedCharArray2d.GetUpperBound(0) do
            yield 
                transposedCharArray2d[row, *]
                |> Seq.map string
                |> String.concat ""
    }
    in
    let problemsAsOneLiners = 
        restringified
        |> splitOnBlankLine
        |> Seq.map (String.concat " ")
    seq {
        for problem in problemsAsOneLiners do
            let numbers = 
                (new Regex "(\d+)").Matches(problem)
                |> Seq.map _.Value
                |> Seq.map int64
            in
            let operation = MathOp.parse ((new Regex "[\+\*]").Match(problem).Value)
            yield { numbers = numbers; operation = operation }
    }
    

module Puzzle = begin
    let part1 (worksheet: Worksheet) =
        worksheet.problems
        |> Seq.sumBy _.evaluate()

    let part2 (input: string array) =
        let problems = 
            input
            |> parseCephalopod
        in
        printfn "Problems: %A" problems;
        problems |> Seq.sumBy _.evaluate()
end
