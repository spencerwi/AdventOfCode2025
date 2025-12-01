module Lib
open System

type Direction = Left | Right

type Rotation = {
    direction : Direction
    amount : int
} with
    static member parse(input : string) : Rotation =
        let direction = 
            match input[0] with
            | 'L' -> Left
            | 'R' -> Right
            | other -> failwithf "Unrecognized direction: %A" other
        in
        let amount = int input[1..] in
        { direction = direction; amount = amount }

    static member left (amount : int) : Rotation =
        { direction = Left; amount = amount }
    static member right (amount : int) : Rotation =
        { direction = Right; amount = amount }

    override this.ToString() : string =
        match this.direction with
        | Left -> "L" + string this.amount
        | Right -> "R" + string this.amount

type State = {
    currentNumber : int
    zeroIndicatedCount : int
    zeroPassedCount : int
} with
    static member Initial = {
        currentNumber = 50;
        zeroIndicatedCount = 0;
        zeroPassedCount = 0
    }

    static member apply (state : State) (rotation : Rotation) =
        // We're gonna wind all the way, and then wind back in chunks of 100 and see how many times we have to go by hundreds before we're back in range. Then we'll correct for times when we went left from zero or right to zero, since the rules say those shouldn't count as "passing" zero. 
        let mutable rawValue = 
            match rotation.direction with
            | Left -> state.currentNumber - rotation.amount
            | Right -> state.currentNumber + rotation.amount
        in
        let mutable zeroPasses = 0;
        if rawValue >= 100 then 
            while rawValue >= 100 do
                rawValue <- rawValue - 100;
                zeroPasses <- zeroPasses + 1;
            if rawValue = 0 then 
                zeroPasses <- zeroPasses - 1
        elif rawValue < 0 then
            while rawValue < 0 do 
                rawValue <- rawValue + 100;
                zeroPasses <- zeroPasses + 1;
            if state.currentNumber = 0 then 
                zeroPasses <- zeroPasses - 1
        let newNumber = rawValue in
        printfn "Moving %A %d steps from %d to %d will pass zero %d times" rotation.direction rotation.amount state.currentNumber newNumber zeroPasses;
        { 
            currentNumber = newNumber
            zeroIndicatedCount = 
                if newNumber = 0 then 
                    state.zeroIndicatedCount + 1 
                else state.zeroIndicatedCount
            zeroPassedCount = state.zeroPassedCount + zeroPasses
        }


module Puzzle = begin
    let solve (input : string seq) =
        let finalState = 
            input
            |> Seq.map Rotation.parse
            |> Seq.fold State.apply State.Initial
        in
        (finalState.zeroIndicatedCount, finalState.zeroIndicatedCount + finalState.zeroPassedCount)
end
