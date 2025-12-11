module Lib
open System.Collections.Generic
open System.Text.RegularExpressions
open FSharp.Collections.ParallelSeq

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
        static member allOff (size : int) =
            { lights = Array.create size Off }

        member this.apply (button : Button) : LightsBank =
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

            member this.isSolutionFor (machine: Machine): bool = 
                this = machine.targetLightsState

        member this.size = this.lights.Length

and JoltageBank = {
    levels : int array
}
    with 
        member this.size = this.levels.Length

        static member zeroes (size : int) =
            { levels = Array.create size 0 }


        member this.apply (button: Button): JoltageBank = 
            {
                levels =
                    this.levels
                    |> Array.mapi (fun idx currentState ->
                        if button.Contains idx then
                            currentState + 1
                        else
                            currentState
                    )
            }

        /// <description>
        /// If any of the joltage levels go over the target, we can ignore that whole state tree, since they'll never go back down.
        /// </description>
        member this.canReachGoal (machine: Machine): bool = 
            seq { 0 .. this.levels.Length - 1}
            |> Seq.forall (fun idx -> this.levels[idx] <= machine.joltageRequirements.levels[idx])

and Machine = {
    targetLightsState : LightsBank
    buttons : Button list
    joltageRequirements : JoltageBank
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
                    joltageRequirements = {levels = joltageRequirements}
                }

module LightsSolver = begin
    type State = {
        lights : LightsBank
        buttonPresses : int
        lastButtonPressed : Button option
    }
        with 
            static member initial (machine : Machine) =
                { 
                    lights = LightsBank.allOff machine.targetLightsState.size
                    buttonPresses = 0
                    lastButtonPressed = None
                }

            member this.press (button : Button) =
                {
                    lights = this.lights.apply button
                    buttonPresses = this.buttonPresses + 1
                    lastButtonPressed = Some button
                }

            member this.isSolvedFor (machine : Machine) =
                this.lights = machine.targetLightsState
                
    let solve (machine : Machine) : int =
        let initialState = State.initial machine in
        let statesToCheck = new PriorityQueue<State, int>() in
        statesToCheck.Enqueue(initialState, 0);
        let mutable foundSolution : State option = None in
        while foundSolution.IsNone do begin
            let current = statesToCheck.Dequeue() in
            if current.isSolvedFor machine then
                foundSolution <- Some current
            else 
                let candidateButtons = 
                    match current.lastButtonPressed with
                    | Some b -> Seq.except [b] machine.buttons // pressing the same button twice is pointless
                    | None -> machine.buttons
                in
                candidateButtons
                |> Seq.map current.press
                |> Seq.iter (fun nextState -> statesToCheck.Enqueue(nextState, nextState.buttonPresses))
        end
        foundSolution.Value.buttonPresses
end

module JoltageSolver = begin
    let solveJoltage (machine : Machine) : int =
        // Linear algebra time! 
        //  Given buttons like: [(3), (1, 3), (2), (2, 3), (0, 2), (0,1)]
        //  And joltage requirements like {3, 5, 4, 7}
        // Our equations are something like:
        //      0(button 1) + 0(button 2) + 0(button 3) + 0(button 4) + 1(button 5) + 1(button 6) = 3
        //      0(button 1) + 1(button 2) + 0(button 3) + 0(button 4) + 0(button 5) + 1(button 6) = 5
        //      0(button 1) + 0(button 2) + 1(button 3) + 1(button 4) + 0(button 5) + 0(button 6) = 4
        //      1(button 1) + 1(button 2) + 0(button 3) + 1(button 4) + 0(button 5) + 0(button 6) = 7
        //
        // Or, as a matrix:
        //      [0, 0, 0, 0, 1, 1 | 3]
        //      [0, 1, 0, 0, 0, 1 | 5]
        //      [0, 0, 1, 1, 0, 0 | 4]
        //      [1, 1, 0, 1, 0, 0 | 7]
        // 
        // We do gaussian elimination, then add the coefficients up!
        // Oh, except that won't work, from reading online. There wind up being free variables in some of the equations. That's really annoying.

        0 // TODO

end

module Puzzle = begin
    let part1 (input : Machine seq) =
        input 
        |> PSeq.map LightsSolver.solve
        |> PSeq.sum 

    let part2 (input: Machine seq) =
        "The right answer" // we'll have to do some linear algebra here
end
