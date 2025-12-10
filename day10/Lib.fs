module Lib
open System.Collections.Generic
open System.Text.RegularExpressions

type Button = Set<int>
let parseButton (input : string) : Button =
    input.Replace("(", "").Replace(")","").Split(",")
    |> Seq.map int
    |> Set.ofSeq
    
type LightState =
    | On
    | Off
    with
        static member parse = function
            | '#' -> On
            | '.' -> Off
            | other -> failwithf "Invalid LightState: %A" other

        member this.toggled() = 
            match this with
            | On -> Off
            | Off -> On

type LightsBank = {
    lights : LightState array
}
    with 
        static member make (arr : LightState array) =
            { lights = arr }
        static member allOff (size : int) =
            { lights = Array.create size Off }

        member this.apply (button : Button) =
            {
                lights = 
                    this.lights
                    |> Array.mapi (fun idx currentState ->
                        if button.Contains idx then
                            currentState.toggled()
                        else
                            currentState
                    )
            }

        member this.size = this.lights.Length

type SolutionState = {
    lights : LightsBank
    buttonPresses : int
}
    with 
        static member initial (numberOfLights : int)=
            { 
                lights = LightsBank.allOff numberOfLights 
                buttonPresses = 0
            }

        member this.press (button : Button) =
            {
                lights = this.lights.apply button
                buttonPresses = this.buttonPresses + 1
            }

        member this.matches (targetState : LightsBank) =
            this.lights = targetState

type Machine = {
    targetLightsState : LightsBank
    buttons : Button list
    joltageRequirements : int array
}
    with 
        static member parse (input : string) : Machine =
            let pattern = new Regex "\[(?<lights>[.#]+)\] (?<buttons>(\((\d+,?)+\)\s*))+\{(?<joltage>(\d+,?)+)\}" in
            let rMatch = pattern.Match(input) in
            if not rMatch.Success then
                failwithf "Invalid machine spec: %s" input
            else
                let lightStates = 
                    rMatch.Groups["lights"].Value
                    |> Seq.map LightState.parse
                    |> Array.ofSeq
                in
                let buttons = 
                    rMatch.Groups["buttons"].Captures
                    |> Seq.map (fun c -> parseButton c.Value)
                    |> List.ofSeq
                in
                let joltageRequirements = 
                    rMatch.Groups["joltage"].Value.Split(",")
                    |> Array.map int
                in
                {
                    targetLightsState = {lights = lightStates}
                    buttons = buttons
                    joltageRequirements = joltageRequirements
                }

        member this.howManyPressesToSolve() : int =
            let initialState = SolutionState.initial this.targetLightsState.size in
            let statesToCheck = new PriorityQueue<SolutionState, int>() in
            statesToCheck.Enqueue(initialState, 0);
            let mutable foundSolution : SolutionState option = None in
            while foundSolution.IsNone do begin
                let current = statesToCheck.Dequeue() in
                if current.matches this.targetLightsState then
                    foundSolution <- Some current
                else
                    this.buttons
                    |> Seq.map current.press
                    |> Seq.iter (fun nextState -> statesToCheck.Enqueue(nextState, nextState.buttonPresses))
            end
            foundSolution.Value.buttonPresses
                    

module Puzzle = begin
    let part1 (input: string seq) =
        input 
        |> Seq.map Machine.parse
        |> Seq.map _.howManyPressesToSolve()
        |> Seq.sum 

    let part2 (input: string seq) =
        "the right answer"
end
