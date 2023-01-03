open System.IO
open System.Text.RegularExpressions
open System.Collections.Generic

let root = __SOURCE_DIRECTORY__
let arr = File.ReadAllLines(Path.Join(root, "/test"))
let lines = Array.toList(arr)

type Node = {
    name: string
    paths: string list
    rate: int
}

type State = {
    location: Node
    released: Node Set
    score: int
    timeLeft: int
}

let parseLine (line:string): Node =
    let name = line[6..7]
    printfn "%s" name
    let rate = int((line.Split(";")[0]).Split("=")[1])
    printfn "%s" name
    let rx = Regex("leads? to valves? ")
    let paths = (rx.Split(line)[1]).Split(", ") |> Array.toList
    { name = name; rate = rate; paths = paths}

let nodes = lines |> List.map parseLine
let start = nodes |> List.find (fun s -> s.name = "AA")
let initState = { location = start; released = [] |> Set; score = 0; timeLeft = 30 }

type NodeScore = { timeLeft: int; score: int; state:State }
let best = [] |> Set<NodeScore>

let winnerTimes = Dictionary<string, NodeScore list>()
for node in nodes do
    winnerTimes.Add(node.name, [])

let getBest (location:string) (timeLeft:int) =
    winnerTimes[location]
        |> List.filter (fun s -> s.timeLeft = timeLeft)

let setBest (location:string) (timeLeft:int) (score:int) state =
    winnerTimes[location] <- {timeLeft=timeLeft; score=score; state=state}::winnerTimes[location]

// let insertWinner (winner:State) =
//     let arr = winnerTimes[winner.location.name]
//     let better =
//         arr
//         |> List.filter (fun s -> s.timeLeft <= winner.timeLeft && s.score >= winner.score)
    
//     if better.IsEmpty then // The winner is the best
//         let newArr =
//             arr
//             |> List.filter (fun s -> s.timeLeft > winner.timeLeft)
//         winnerTimes[winner.location.name] <- { timeLeft = winner.timeLeft; score = winner.score }::arr
//     else
//         ()

let rec update (state: State): State =
    if state.timeLeft = 0 then
        state
    else
        let prior = getBest state.location.name state.timeLeft
        if List.isEmpty prior = false then
            prior[0].state
        else

        let mutable options = []
        if state.released.Contains state.location = false then
            let s1 = { 
                state with
                    released = state.released.Add state.location
                    score = state.score + state.location.rate * state.timeLeft
                    timeLeft = state.timeLeft - 1
                }
            let o1 = update s1
            options <- o1::options
        else
            ()
        
        for child in state.location.paths do
            let cNode = nodes |> List.find (fun n -> n.name = child )
            let s = {
                state with
                    location = cNode
                    timeLeft = state.timeLeft - 1
                }
            let o = update s
            options <- o::options
        
        let winner = options |> List.maxBy (fun s -> s.score )
        setBest state.location.name state.timeLeft winner.score winner
        winner

let partOne = update initState