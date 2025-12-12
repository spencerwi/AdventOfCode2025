module Lib
open System
open FSharp.Collections.ParallelSeq

type DeviceName = string

type Path = string list

// memoization helper to speed up
let memoize f =
    let cache = new System.Collections.Generic.Dictionary<_, _>();
    (fun x -> 
        match cache.TryGetValue x with
        | true, result -> result
        | _ -> 
            let result = f x
            cache.Add(x, result)
            result
    )
        

type ServerRack = {
    deviceConnections : Map<DeviceName, Set<DeviceName>>
}
    with
        static member parse (input : string seq) : ServerRack =
            let deviceConnections = 
                input
                |> Seq.map (fun line -> line.Split(":", StringSplitOptions.TrimEntries ||| StringSplitOptions.RemoveEmptyEntries))
                |> Seq.map (fun ([|name; connectionsStr|]) -> (name, connectionsStr.Split(" ", StringSplitOptions.RemoveEmptyEntries) |> set))
                |> Map.ofSeq
            in
            {
                deviceConnections =  deviceConnections
            }

        member this.Item
            with get (device : DeviceName) = this.deviceConnections[device]

        member this.hasConnectionsFrom (device : DeviceName) =
            this.deviceConnections.ContainsKey device

let rec findPathsBetween = memoize (fun (rack : ServerRack, start : DeviceName, goal : DeviceName) ->
        if start = "out" then 
            [["out"]]
        elif not (rack.hasConnectionsFrom start) then
            printfn "%s is terminal, no more connections" start
            []
        elif rack[start] = set ["out"] then
            [[start; "out"]]
        else
            rack[start]
            |> PSeq.collect (fun next -> findPathsBetween (rack, next, goal))
            |> PSeq.map (fun path -> List.append [start] path)
            |> List.ofSeq
    )
    
module Puzzle = begin
    let part1 (serverRack: ServerRack) =
        findPathsBetween (serverRack, "you", "out")
        |> Set.ofSeq
        |> Set.count

    let part2 (serverRack: ServerRack) =
        // Maybe I should find paths between "svr" and "fft", "fft" to "dac", and then "dac" to "out" (although the fft/dac can be swapped)
        // nevermind, that's too slow
        //findPathsBetween (serverRack, "svr", "out")
        //|> Set.ofSeq
        //|> Set.filter (fun path -> Seq.contains "fft" path && Seq.contains "dac" path)
        //|> Set.count
        0L // TODO: a sufficiently-performant solution
end
