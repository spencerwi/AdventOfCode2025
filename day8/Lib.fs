module Lib

let allUniquePairs (xs : 'a seq) : ('a * 'a) seq =
    seq {
        let arr = Array.ofSeq xs in 
        for i in 0 .. arr.Length - 2 do
            for j in i+1 .. arr.Length - 1 do
                yield (arr[i], arr[j])
    }

[<Struct>]
type JunctionBox = {
    x : int64
    y : int64
    z : int64
}
    with 
        static member parse (line : string) : JunctionBox =
            let [|x;y;z|] = 
                line.Trim().Split(",")
                |> Array.map int64
            in
            {x = x; y = y; z = z}

        member this.distanceTo (other : JunctionBox) =
            let dx = float (other.x - this.x)
            let dy = float (other.y - this.y)
            let dz = float (other.z - this.z)
            sqrt ((dx * dx) + (dy * dy) + (dz * dz))

type Circuit = {
    mutable boxes : Set<JunctionBox>
}
    with 
        static member make (a : JunctionBox, b : JunctionBox) = 
            {boxes = Set.ofList [a; b]}

        static member merge (circuitA : Circuit) (circuitB : Circuit) =
            {
                boxes = Set.union circuitA.boxes circuitB.boxes
            }
        member this.size = this.boxes.Count
        member this.add (box : JunctionBox) = this.boxes <- this.boxes.Add box
        member this.contains (box : JunctionBox)  = this.boxes.Contains box

type Solution = int64 * int64

let connect (boxes : JunctionBox seq) (measureAt : int) : Solution =
    let pairsWithDistances = 
        boxes
        |> allUniquePairs
        |> Seq.map (fun (a, b) -> (a.distanceTo b, a, b))
        |> Seq.sortBy (fun (distance, _, _) -> distance)
        |> Array.ofSeq
    in
    let mutable circuits : Circuit array = Array.empty in
    let mutable lastConnectedPair : (JunctionBox * JunctionBox) option = None in
    let circuitFor box =
        circuits
        |> Seq.tryFindIndex (fun c -> c.contains box)
    in
    let doConnection idx =
        let (distance, a, b) = pairsWithDistances[idx] in
        // printf "At a distance of %f; %0A and %0A are the closest two neighbors" distance a b
        match circuitFor a, circuitFor b with
            | (Some aCircuit, Some bCircuit) when aCircuit = bCircuit ->
                // printfn "...but they're already connected."
                ()
            | (Some aCircuit, Some bCircuit) ->
                // printfn "...and they're each on different circuits. Connecting those two circuits!"
                let connected = Circuit.merge circuits[aCircuit] circuits[bCircuit] in
                circuits[aCircuit] <- connected
                circuits <- Array.removeAt bCircuit circuits
                lastConnectedPair <- Some (a, b)
            | (Some aCircuit, None) ->
                // printfn ". Connecting %0A to the circuit for %0A" b a;
                circuits[aCircuit].add b;
                lastConnectedPair <- Some (a, b)
            | (None, Some bCircuit) ->
                // printfn ". Connecting %0A to the circuit for %0A" a b;
                circuits[bCircuit].add a;
                lastConnectedPair <- Some (a, b)
            | (None, None) ->
                // printfn ". Connecting them on a new circuit";
                circuits <- Array.append circuits [|Circuit.make (a, b)|];
                lastConnectedPair <- Some (a, b)
    in
    for idx in 0..(measureAt - 1) do begin
        doConnection idx
    end
    let part1 = 
        circuits
        |> Seq.map _.size
        |> Seq.sortDescending
        |> Seq.take 3
        |> Seq.fold (*) 1
    in
    for idx in measureAt..(pairsWithDistances.Length - 1) do begin
        doConnection idx
    end
    let (a, b) = lastConnectedPair.Value in
    let part2 = a.x * b.x in
    (part1, part2)
        

    

module Puzzle = begin
    let solve (connectionLimitForPart1) (input : string seq) =
        let boxes = 
            input
            |> Seq.map JunctionBox.parse
        in
        connect boxes connectionLimitForPart1 
end
