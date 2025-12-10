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

type ISolutionTarget<'Self> = 
    static abstract forMachine : Machine -> 'Self
    abstract apply : Button -> 'Self
    abstract isSolutionFor : Machine -> bool
    /// <description>
    /// Useful for shortcutting states that cannot reach the goal
    /// </description>
    abstract canReachGoal : Machine -> bool

and LightsBank = {
    lights : LightState array
}
    with 
        static member allOff (size : int) =
            { lights = Array.create size Off }

        interface ISolutionTarget<LightsBank> with
            static member forMachine (machine : Machine) =
                LightsBank.allOff machine.targetLightsState.size

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

            member _.canReachGoal (machine: Machine): bool = 
                true

        member this.size = this.lights.Length

and JoltageBank = {
    levels : int array
}
    with 
        member this.size = this.levels.Length


        interface ISolutionTarget<JoltageBank> with
            static member forMachine (machine : Machine) = 
                { levels = Array.create machine.joltageRequirements.size 0 }

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

            member this.isSolutionFor (machine: Machine): bool = 
                this = machine.joltageRequirements

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

type SolutionState<'Target when 'Target :> ISolutionTarget<'Target>>  = {
    state : 'Target
    buttonPresses : int
}
    with 
        static member initial (machine : Machine) =
            { 
                state = 'Target.forMachine machine
                buttonPresses = 0
            }

        member this.press (button : Button) =
            {
                state = this.state.apply button
                buttonPresses = this.buttonPresses + 1
            }

        member this.isSolvedFor (machine : Machine) =
            this.state.isSolutionFor machine

        member this.canReachGoalFor (machine : Machine) =
            this.state.canReachGoal machine


let solveForLightState (machine : Machine) : int =
    let initialState = SolutionState<LightsBank>.initial machine in
    let statesToCheck = new PriorityQueue<SolutionState<LightsBank>, int>() in
    statesToCheck.Enqueue(initialState, 0);
    let mutable foundSolution : SolutionState<LightsBank> option = None in
    while foundSolution.IsNone do begin
        let current = statesToCheck.Dequeue() in
        if current.isSolvedFor machine then
            foundSolution <- Some current
        else
            machine.buttons
            |> Seq.map current.press
            |> Seq.iter (fun nextState -> statesToCheck.Enqueue(nextState, nextState.buttonPresses))
    end
    foundSolution.Value.buttonPresses


let solve<'a when 'a :> ISolutionTarget<'a>> (machine : Machine) : int =
    let initialState = SolutionState<'a>.initial machine in
    let statesToCheck = new PriorityQueue<SolutionState<'a>, int>() in
    statesToCheck.Enqueue(initialState, 0);
    let mutable foundSolution : SolutionState<'a> option = None in
    while foundSolution.IsNone do begin
        let current = statesToCheck.Dequeue() in
        if current.isSolvedFor machine then
            foundSolution <- Some current
        elif current.canReachGoalFor machine then
            machine.buttons
            |> Seq.map current.press
            |> Seq.iter (fun nextState -> statesToCheck.Enqueue(nextState, nextState.buttonPresses))
    end
    foundSolution.Value.buttonPresses



module Puzzle = begin
    let part1 (input : Machine seq) =
        input 
        |> PSeq.map solve<LightsBank>
        |> PSeq.sum 

    let part2 (input: Machine seq) =
        input 
        |> PSeq.map solve<JoltageBank>
        |> PSeq.sum 
end
